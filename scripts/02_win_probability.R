# ==============================================================================
# DAY 3A: WIN PROBABILITY MODEL
# Purpose: Build a model to predict P(Win) based on Score, Hammer, and End
# ==============================================================================

library(tidyverse)
library(broom)

# 1. LOAD DATA
# Games (for results) and Ends (for scoring history)
games <- read_csv("data/raw/Games.csv", show_col_types = FALSE) %>%
  mutate(match_id = paste(CompetitionID, SessionID, GameID, sep = "_"))

ends <- read_csv("data/raw/Ends.csv", show_col_types = FALSE) %>%
  mutate(match_id = paste(CompetitionID, SessionID, GameID, sep = "_"))

print(paste("Loaded", nrow(games), "games and", nrow(ends), "ends."))

# 2. DETERMINE WINNERS
# To know who won every match to train the model.
# Logic: Games.csv 'Winner' col: 1 = Team1 won, 0 = Team2 won.

winners <- games %>%
  select(match_id, TeamID1, TeamID2, Winner) %>%
  pivot_longer(cols = c(TeamID1, TeamID2), 
               names_to = "team_pos", 
               values_to = "TeamID") %>%
  mutate(
    # If I am Team1 and Winner is 1, I won.
    # If I am Team2 and Winner is 0, I won.
    won = case_when(
      team_pos == "TeamID1" & Winner == 1 ~ 1,
      team_pos == "TeamID2" & Winner == 0 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  select(match_id, TeamID, won)

# 3. RECONSTRUCT GAME STATE (Re-using Day 2S Logic)

# A) Calculate Scores
scores <- ends %>%
  group_by(match_id, TeamID) %>%
  arrange(EndID) %>%
  mutate(my_score_before = cumsum(lag(Result, default = 0))) %>%
  ungroup()

# B) Calculate Score Differential
game_states <- scores %>%
  left_join(scores, by = c("match_id", "EndID"), suffix = c("", "_opp"), relationship = "many-to-many") %>%
  filter(TeamID != TeamID_opp) %>%
  mutate(score_diff = my_score_before - my_score_before_opp) %>%
  select(match_id, EndID, TeamID, score_diff)

# C) Calculate Hammer Logic
# (Simplified version of Day 2S for speed, assuming data integrity)
hammer_info <- games %>%
  select(match_id, TeamID1, TeamID2, LSFE)

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
    ends_remaining = 8 - EndID + 1 # +1 because we are at the START of the end
  ) %>%
  # Filter out extra ends (End > 8) for standard WP model
  filter(EndID <= 8) %>%
  select(match_id, EndID, TeamID, has_hammer, ends_remaining)

# 4. CREATE TRAINING DATA
# Join States + Hammer + Outcome
training_data <- game_states %>%
  left_join(hammer_logic, by = c("match_id", "EndID", "TeamID")) %>%
  left_join(winners, by = c("match_id", "TeamID")) %>%
  filter(!is.na(won)) %>%         # Remove games with no result
  filter(!is.na(has_hammer))      # Remove messy hammer data

print(paste("Training Data Ready:", nrow(training_data), "rows."))

# 5. TRAIN WIN PROBABILITY MODEL (Logistic Regression)
# Formula: P(Win) ~ Score Diff + Hammer + Ends Remaining + Interactions
# Interactions (*) allow the value of the hammer to change depending on how many ends are left.

wp_model <- glm(won ~ score_diff * has_hammer * ends_remaining, 
                data = training_data, 
                family = binomial)

print(summary(wp_model))

# Quick check: What is P(Win) if 'I' am Tied, with Hammer, in End 8 (1 end remaining)?
test_case <- data.frame(score_diff = 0, has_hammer = TRUE, ends_remaining = 1)
prob <- predict(wp_model, newdata = test_case, type = "response")
print(paste("Win Prob (Tied, Hammer, Last End):", round(prob*100, 1), "%"))

# Save model for future use
saveRDS(wp_model, "wp_model.rds")