# ==============================================================================
# DAY 4A: WIN PROBABILITY MODEL V2 (Refined & Validated)
# Purpose: Train model, VALIDATE fit (AUC/Calibration), and generate predictions
# ==============================================================================

library(tidyverse)
library(broom)
library(pROC)

# --- 1. LOAD DATA ---
games <- read_csv("data/raw/Games.csv", show_col_types = FALSE) %>%
  mutate(match_id = paste(CompetitionID, SessionID, GameID, sep = "_"))

ends <- read_csv("data/raw/Ends.csv", show_col_types = FALSE) %>%
  mutate(match_id = paste(CompetitionID, SessionID, GameID, sep = "_"))

# --- 2. DETERMINE WINNERS ---
winners <- games %>%
  select(match_id, TeamID1, TeamID2, Winner) %>%
  pivot_longer(cols = c(TeamID1, TeamID2), names_to = "team_pos", values_to = "TeamID") %>%
  mutate(won = case_when(
    team_pos == "TeamID1" & Winner == 1 ~ 1,
    team_pos == "TeamID2" & Winner == 0 ~ 1,
    TRUE ~ 0
  )) %>%
  select(match_id, TeamID, won)

# --- 3. RECONSTRUCT GAME STATE (Enhanced) ---
# A) Calculate Scores
scores <- ends %>%
  group_by(match_id, TeamID) %>%
  arrange(EndID) %>%
  mutate(my_score_before = cumsum(lag(Result, default = 0))) %>%
  ungroup()

# B) Hammer Logic
hammer_info <- games %>% select(match_id, TeamID1, TeamID2, LSFE)

hammer_logic <- ends %>%
  left_join(ends %>% select(match_id, EndID, TeamID, Result), 
            by = c("match_id", "EndID"), suffix = c("", "_opp"), relationship = "many-to-many") %>%
  filter(TeamID != TeamID_opp) %>%
  left_join(hammer_info, by = "match_id") %>%
  group_by(match_id, TeamID) %>%
  arrange(EndID) %>%
  mutate(
    hammer_owner_raw = case_when(
      EndID == 1 & LSFE == 1 ~ TeamID1,
      EndID == 1 & LSFE == 0 ~ TeamID2,
      lag(Result, default=0) > 0 ~ TeamID_opp,
      lag(Result_opp, default=0) > 0 ~ TeamID,
      TRUE ~ NA_real_
    )
  ) %>%
  fill(hammer_owner_raw, .direction = "down") %>%
  mutate(
    has_hammer = (TeamID == hammer_owner_raw),
    ends_remaining = 8 - EndID + 1
  ) %>%
  filter(EndID <= 8) %>%
  select(match_id, EndID, TeamID, has_hammer, ends_remaining)

# C) TRAINING DATA (Now including PowerPlay)
# We need the 'PowerPlay' column to measure its impact later.
training_data <- scores %>%
  left_join(scores, by = c("match_id", "EndID"), suffix = c("", "_opp"), relationship = "many-to-many") %>%
  filter(TeamID != TeamID_opp) %>%
  mutate(score_diff = my_score_before - my_score_before_opp) %>%
  # *** STRATEGIC UPDATE: Select PowerPlay here ***
  select(match_id, EndID, TeamID, score_diff, PowerPlay) %>% 
  left_join(hammer_logic, by = c("match_id", "EndID", "TeamID")) %>%
  left_join(winners, by = c("match_id", "TeamID")) %>%
  filter(!is.na(won), !is.na(has_hammer))

print(paste("Training Data Ready:", nrow(training_data), "rows."))

# --- 4. TRAIN & EVALUATE MODEL ---
# Formula: P(Win) ~ Score Diff + Hammer + Ends Remaining + Interactions
# We stick to this specification because "Physics of Curling" don't change.
wp_model <- glm(won ~ score_diff * has_hammer * ends_remaining, 
                data = training_data, family = binomial)

# A) Generate Probabilities
training_data$wp_pred <- predict(wp_model, newdata = training_data, type = "response")

# B) Check AUC (The "Judge's Metric")
roc_obj <- roc(training_data$won, training_data$wp_pred)
auc_val <- auc(roc_obj)

print("---------------------------------------------------")
print(paste("MODEL ACCURACY (AUC):", round(auc_val, 4)))
print("Interpretation: >0.80 is Competition Standard.")
print("---------------------------------------------------")

# C) Calibration Check (The "Reality Check")
# We group predicted probabilities into bins (0-10%, 10-20%...) and see if they actually won that often.
calibration <- training_data %>%
  mutate(prob_bin = cut(wp_pred, breaks = seq(0, 1, 0.1))) %>%
  group_by(prob_bin) %>%
  summarize(
    predicted_avg = mean(wp_pred),
    actual_win_rate = mean(won),
    n = n()
  )

print("Calibration Table (Does Predicted match Actual?):")
print(calibration)

# --- 5. CALCULATE POWER PLAY IMPACT (WPA) ---
# WPA = Probability AFTER the end - Probability BEFORE the end
# We need to know the state of the *next* end to know the "After" probability.

# Lookup table for the next end's probability
next_end_lookup <- training_data %>%
  mutate(prev_end = EndID - 1) %>%
  select(match_id, TeamID, prev_end, wp_next = wp_pred)

final_dataset <- training_data %>%
  left_join(next_end_lookup, by = c("match_id", "TeamID", "EndID" = "prev_end")) %>%
  mutate(
    # If End 8 (Last End), the 'next' probability is the actual result (1 or 0)
    wp_end = ifelse(EndID == 8, won, wp_next),
    
    # Calculate Impact
    wpa = wp_end - wp_pred
  )

# --- 6. SAVE OUTPUTS ---
saveRDS(final_dataset, "wp_predictions_v2.rds")
saveRDS(wp_model, "wp_model_v2.rds")
print("Model V2 and Predictions saved successfully.")