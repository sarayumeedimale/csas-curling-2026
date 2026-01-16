# ==============================================================================
# DAY 12: PREDICTIVE MODEL (REFINED & STABILIZED)
# Objective: Fix "Perfect Separation" by grouping rare shot types
# ==============================================================================

library(tidyverse)
library(broom)
library(pROC)

# 1. LOAD DATA & RECOVER SCORES -------------------------------------------
pp_data <- readRDS("pp_analysis_ready.rds")

# --- RECOVERY BLOCK (Just to be safe) ---
raw_ends <- read_csv("data/raw/Ends.csv", show_col_types = FALSE) %>%
  mutate(match_id = paste(CompetitionID, SessionID, GameID, sep = "_")) %>%
  select(match_id, EndID, TeamID, Result)

if("Result" %in% colnames(pp_data)) pp_data <- select(pp_data, -Result)

pp_data <- pp_data %>%
  left_join(raw_ends, by = c("match_id", "EndID", "TeamID")) %>%
  mutate(match_id = as.character(match_id), TeamID = as.character(TeamID))

stones <- read_csv("data/raw/Stones.csv", show_col_types = FALSE) %>%
  mutate(
    match_id = paste(CompetitionID, SessionID, GameID, sep = "_"),
    match_id = as.character(match_id), 
    TeamID = as.character(TeamID),
    EndID = as.numeric(EndID)
  )

print("Data Loaded.")

# 2. SIMPLIFY SHOT TYPES (The Fix) ----------------------------------------
# We create 3 broad categories to prevent the "Infinite Odds" error
stones <- stones %>%
  mutate(
    simple_type = case_when(
      # Defensive / Setup
      Task %in% c(1, 2, 4, 5) ~ "Guard_Setup",  # Front, Guard, Wick, Freeze
      # Offensive / Scoring
      Task %in% c(0, 3, 10)   ~ "Draw_Offense", # Draw, Raise, Promotion
      # Clearing / Defense
      Task %in% c(6, 7, 8, 9) ~ "Takeout",      # Takeout, HitRoll, Clearing, Double
      TRUE ~ "Other"
    )
  )

# 3. PREPARE MODEL DATA ---------------------------------------------------
early_shots <- stones %>%
  inner_join(
    pp_data %>% 
      filter(PowerPlay == 1) %>% 
      select(match_id, EndID, TeamID, pressure_combined, Result),
    by = c("match_id", "EndID", "TeamID")
  ) %>%
  arrange(match_id, EndID, TeamID, ShotID) %>%
  group_by(match_id, EndID, TeamID) %>%
  mutate(shot_order = row_number()) %>%
  filter(shot_order <= 2) %>% # Look at first 2 shots only
  ungroup()

model_data <- early_shots %>%
  select(match_id, EndID, TeamID, shot_order, simple_type, pressure_combined, Result) %>%
  pivot_wider(
    names_from = shot_order,
    values_from = simple_type,
    names_prefix = "shot"
  ) %>%
  mutate(
    # Define Success (2+ points)
    success = as.factor(ifelse(Result >= 2, 1, 0)),
    # Ensure factors have reference levels
    shot1 = as.factor(shot1),
    shot2 = as.factor(shot2),
    pressure_combined = as.factor(pressure_combined)
  ) %>%
  drop_na()

print("Model Data Prepared with Simplified Categories.")

# 4. RUN STABILIZED MODEL -------------------------------------------------
pp_model <- glm(
  success ~ shot1 + shot2 + pressure_combined,
  data = model_data,
  family = binomial
)

print("=== MODEL SUMMARY (Stable) ===")
print(tidy(pp_model))

# 5. INTERPRETATION -------------------------------------------------------
coefficients <- tidy(pp_model) %>%
  mutate(
    odds_ratio = exp(estimate),
    prob_impact = (odds_ratio - 1) * 100, # % increase/decrease in odds
    significance = case_when(
      p.value < 0.05 ~ "***",
      p.value < 0.10 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(term, odds_ratio, prob_impact, significance, p.value) %>%
  arrange(p.value)

print("=== KEY PREDICTORS ===")
print(coefficients)

# 6. MODEL ACCURACY -------------------------------------------------------
# Predictions
model_data$prob <- predict(pp_model, type = "response")
model_data$pred <- ifelse(model_data$prob > 0.5, 1, 0)

# Confusion Matrix
conf_matrix <- table(Predicted = model_data$pred, Actual = model_data$success)
print("=== CONFUSION MATRIX ===")
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Model Accuracy:", round(accuracy, 3)))

# ROC / AUC
roc_curve <- roc(model_data$success, model_data$prob)
print(paste("AUC Score:", round(auc(roc_curve), 3)))

# 7. SAVE -----------------------------------------------------------------
saveRDS(pp_model, "final_predictive_model.rds")
saveRDS(coefficients, "model_insights.rds")