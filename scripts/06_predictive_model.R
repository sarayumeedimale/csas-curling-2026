# ==============================================================================
# DAY 12S: PREDICTIVE MODEL v1 (Sarayu)
# Objective: Can we predict power play success from early shots + pressure?
# ==============================================================================

library(tidyverse)
library(broom)

# 1. LOAD DATA
pp_data <- readRDS("pp_analysis_ready.rds")
stones <- read_csv("data/raw/Stones.csv", show_col_types = FALSE)

# 2. PREPARE SHOT DATA
stones <- stones %>%
  mutate(
    match_id = paste(CompetitionID, SessionID, GameID, sep = "_"),
    shot_type = case_when(
      Task == 0 ~ "Draw",
      Task == 1 ~ "Front",
      Task == 2 ~ "Guard",
      Task == 3 ~ "Raise",
      Task == 4 ~ "Wick",
      Task == 5 ~ "Freeze",
      Task == 6 ~ "Takeout",
      Task == 7 ~ "HitRoll",
      Task == 8 ~ "Clearing",
      Task == 9 ~ "Double",
      Task == 10 ~ "Promotion",
      TRUE ~ "Other"
    )
  )

# 3. GET FIRST 3 SHOTS PER POWER PLAY END
early_shots <- stones %>%
  inner_join(
    pp_data %>% filter(PowerPlay == 1) %>% select(match_id, EndID, TeamID, pressure_combined, Result),
    by = c("match_id", "EndID", "TeamID")
  ) %>%
  arrange(match_id, EndID, TeamID, ShotID) %>%
  group_by(match_id, EndID, TeamID) %>%
  mutate(shot_order = row_number()) %>%
  filter(shot_order <= 3) %>%
  ungroup()

# 4. PIVOT TO WIDE FORMAT (one row per power play)
model_data <- early_shots %>%
  select(match_id, EndID, TeamID, shot_order, shot_type, pressure_combined, Result) %>%
  pivot_wider(
    names_from = shot_order,
    values_from = shot_type,
    names_prefix = "shot"
  ) %>%
  mutate(
    success = as.factor(ifelse(Result >= 2, "Yes", "No"))
  ) %>%
  filter(!is.na(shot1), !is.na(shot2))  # Need at least 2 shots

# 5. BUILD PREDICTIVE MODEL
pp_model <- glm(
  success ~ shot1 + shot2 + pressure_combined,
  data = model_data,
  family = binomial
)

print("=== MODEL SUMMARY ===")
print(summary(pp_model))

# 6. TIDY COEFFICIENTS
coefficients <- tidy(pp_model) %>%
  mutate(
    odds_ratio = exp(estimate),
    significant = p.value < 0.05
  ) %>%
  arrange(p.value)

print("=== COEFFICIENTS (sorted by significance) ===")
print(coefficients)

# 7. SAVE OUTPUTS
saveRDS(pp_model, "predictive_model.rds")
saveRDS(coefficients, "model_coefficients.rds")
saveRDS(model_data, "model_data.rds")

print("✓ Saved: predictive_model.rds")
print("✓ Saved: model_coefficients.rds")


# ==============================================================================
# DAY 12A: MODEL EVALUATION & INTERPRETATION (Armaan)
# ==============================================================================

library(pROC)

# 1. LOAD MODEL (if starting fresh)
# pp_model <- readRDS("predictive_model.rds")
# model_data <- readRDS("model_data.rds")

# 2. GET PREDICTIONS
model_data$predicted_prob <- predict(pp_model, type = "response")
model_data$predicted_class <- ifelse(model_data$predicted_prob >= 0.5, "Yes", "No")

# 3. CONFUSION MATRIX
conf_matrix <- table(
  Predicted = model_data$predicted_class,
  Actual = model_data$success
)
print("=== CONFUSION MATRIX ===")
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", round(accuracy, 3)))

# 4. ROC CURVE & AUC
roc_obj <- roc(model_data$success, model_data$predicted_prob)
auc_value <- auc(roc_obj)
print(paste("AUC:", round(auc_value, 3)))

# 5. INTERPRETATION: WHICH SHOTS PREDICT SUCCESS?
print("=== KEY FINDINGS ===")

coefficients <- readRDS("model_coefficients.rds")

# Positive effects (increase success odds)
positive_effects <- coefficients %>%
  filter(estimate > 0, term != "(Intercept)") %>%
  arrange(desc(odds_ratio))

print("Shots that INCREASE success probability:")
print(positive_effects %>% select(term, odds_ratio, p.value))

# Negative effects (decrease success odds)
negative_effects <- coefficients %>%
  filter(estimate < 0) %>%
  arrange(odds_ratio)

print("Shots that DECREASE success probability:")
print(negative_effects %>% select(term, odds_ratio, p.value))

# 6. PRESSURE EFFECT
pressure_coef <- coefficients %>% filter(str_detect(term, "pressure"))
print("=== PRESSURE EFFECT ===")
print(pressure_coef %>% select(term, odds_ratio, p.value))

# 7. INTERACTION MODEL (Does shot effect change under pressure?)
interaction_model <- glm(
  success ~ shot1 * pressure_combined,
  data = model_data,
  family = binomial
)

print("=== INTERACTION MODEL (Shot1 x Pressure) ===")
interaction_coefs <- tidy(interaction_model) %>%
  filter(str_detect(term, ":")) %>%
  mutate(odds_ratio = exp(estimate)) %>%
  arrange(p.value)
print(interaction_coefs %>% select(term, odds_ratio, p.value))

# 8. SAVE FINAL OUTPUTS
model_evaluation <- list(
  accuracy = accuracy,
  auc = as.numeric(auc_value),
  confusion_matrix = conf_matrix,
  roc = roc_obj
)
saveRDS(model_evaluation, "model_evaluation.rds")
saveRDS(interaction_model, "interaction_model.rds")

print("✓ Saved: model_evaluation.rds")
print("✓ Saved: interaction_model.rds")

