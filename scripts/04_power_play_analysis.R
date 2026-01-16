# scripts/04_power_play_analysis.R
# Day 8S: Core Analysis 1 - Scoring Under Pressure

library(tidyverse)
library(broom) # For tidy statistical test outputs

# 1. LOAD DATA ------------------------------------------------------------
# --- FIX: RECOVER MISSING SCORE DATA (CORRECTED) ---

# 1. READ THE RAW FILE FIRST (This is the missing line!)
raw_ends <- read_csv("data/raw/Ends.csv", show_col_types = FALSE)

# 2. NOW create the matching ID
raw_ends <- raw_ends %>%
  mutate(match_id = paste(CompetitionID, SessionID, GameID, sep = "_")) %>%
  select(match_id, EndID, TeamID, Result) 

# 3. Join it back to your data
# Safety check: Remove 'Result' if it already exists to avoid duplicates
if("Result" %in% colnames(pp_data)) {
  pp_data <- pp_data %>% select(-Result)
}

pp_data <- pp_data %>%
  left_join(raw_ends, by = c("match_id", "EndID", "TeamID"))

# 4. Check that it worked (Result should be numeric now)
print("Data Recovery Complete. Columns:")
print(colnames(pp_data))

# 3. Join it back to your data
# We use 'right_join' or 'left_join' depending on needs, but here left_join is safe.
# Note: Since both have 'Result', we need to be careful not to duplicate.
# We will drop the old (missing) Result column if it exists, then join.

if("Result" %in% colnames(pp_data)) {
  pp_data <- pp_data %>% select(-Result)
}

pp_data <- pp_data %>%
  left_join(raw_ends, by = c("match_id", "EndID", "TeamID"))

# Quick check
colnames(pp_data)
# ---------------------------------------
# ---------------------------------------
# 2. ANALYSIS: SCORING SUMMARY --------------------------------------------
# We want to know: Do average points drop when pressure rises?
# We also check "% Success" (Scoring 2+ points is the goal of a Power Play)

scoring_summary <- pp_data %>%
  group_by(pressure_combined) %>%
  summarize(
    n_ends = n(),
    mean_points = mean(Result, na.rm = TRUE),
    success_rate_2plus = mean(Result >= 2, na.rm = TRUE),
    big_end_rate_3plus = mean(Result >= 3, na.rm = TRUE)
  )

print("--- SCORING PERFORMANCE BY PRESSURE ---")
print(scoring_summary)


# 3. STATISTICAL TEST (t-test) --------------------------------------------
# Question: Is the difference between "Low" and "High" pressure significant?
# We filter to just those two groups for a clean comparison.

test_data <- pp_data %>%
  filter(pressure_combined %in% c("low", "high"))

# We use a t-test to compare means
t_test_result <- t.test(Result ~ pressure_combined, data = test_data)

print("--- T-TEST RESULTS (Low vs High Pressure) ---")
print(t_test_result)


# 4. SAVE RESULTS ---------------------------------------------------------
# We'll save the summary table to look at later for the report
saveRDS(scoring_summary, "scoring_summary_table.rds")


# ==============================================================================
# DAY 8A: SHOT SELECTION ANALYSIS (Armaan) - LOGIC FIX
# Objective: Do teams play more conservatively (more guards) under pressure?
# ==============================================================================

# 5. PREPARE SHOT DATA ----------------------------------------------------
stones <- read_csv("data/raw/Stones.csv", show_col_types = FALSE) %>%
  mutate(match_id = paste(CompetitionID, SessionID, GameID, sep = "_")) %>%
  mutate(
    match_id = as.character(match_id),
    TeamID = as.character(TeamID),
    EndID = as.numeric(EndID),
    ShotID = as.numeric(ShotID) 
  )

pressure_tags <- pp_data %>%
  select(match_id, EndID, TeamID, pressure_combined) %>%
  distinct() %>%
  mutate(
    match_id = as.character(match_id),
    TeamID = as.character(TeamID),
    EndID = as.numeric(EndID)
  )

pp_shots <- stones %>%
  inner_join(pressure_tags, by = c("match_id", "EndID", "TeamID"))

# --- TRANSLATE TASK CODES ---
pp_shots <- pp_shots %>%
  mutate(shot_type_name = case_when(
    Task == 0 ~ "Draw",
    Task == 1 ~ "Front",
    Task == 2 ~ "Guard",
    Task == 3 ~ "Raise",
    Task == 4 ~ "Wick",
    Task == 5 ~ "Freeze",
    Task == 6 ~ "Take-out",
    Task == 7 ~ "Hit and Roll",
    Task == 8 ~ "Clearing",
    Task == 9 ~ "Double Take-out",
    TRUE ~ "Other"
  ))

# 6. ANALYSIS: EARLY END STRATEGY (Shots 1-3) -----------------------------
# FIX: Create a "shot_order" that resets to 1 for every End/Team combo
shot_strategy <- pp_shots %>%
  arrange(match_id, EndID, TeamID, ShotID) %>%  # Ensure strict time order
  group_by(match_id, EndID, TeamID) %>%
  mutate(shot_order = row_number()) %>%         # Number them 1, 2, 3, 4, 5...
  ungroup() %>%
  filter(shot_order <= 3) %>%                   # NOW filter for the first 3
  
  # Calculate percentages
  group_by(pressure_combined, shot_type_name) %>%
  summarize(n_shots = n(), .groups = "drop") %>%
  group_by(pressure_combined) %>%
  mutate(pct = n_shots / sum(n_shots)) %>%
  arrange(pressure_combined, desc(pct))

print("--- SHOT SELECTION (First 3 Stones) BY PRESSURE ---")
print(shot_strategy)

# 7. DRILL DOWN: GUARD USAGE ----------------------------------------------
guard_usage <- shot_strategy %>%
  filter(shot_type_name == "Guard") %>%
  select(pressure_combined, shot_type_name, n_shots, pct)

print("--- GUARD USAGE % UNDER PRESSURE ---")
print(guard_usage)

# 8. SAVE RESULTS ---------------------------------------------------------
saveRDS(shot_strategy, "shot_strategy_summary.rds")

# ==============================================================================
# DAY 9S: EXECUTION QUALITY ANALYSIS (Sarayu)
# Objective: Does shot execution (0-4 score) drop under high pressure?
# ==============================================================================

# 9. EXECUTION ANALYSIS ---------------------------------------------------
# We continue using the 'pp_shots' dataframe created in Day 8A.
# It contains the 'Points' column (raw execution score 0-4).

# Calculate Mean Score and "Perfect Shot" % for each pressure level
execution_summary <- pp_shots %>%
  group_by(pressure_combined) %>%
  summarize(
    avg_score = mean(Points, na.rm = TRUE),
    pct_perfect = mean(Points == 4, na.rm = TRUE),
    sample_size = n()
  ) %>%
  arrange(pressure_combined)

print("--- EXECUTION QUALITY (Avg Score 0-4) BY PRESSURE ---")
print(execution_summary)

# 10. STATISTICAL TEST (ANOVA) --------------------------------------------
# We use ANOVA to see if the differences in means are statistically significant.
# Null Hypothesis: Pressure has no effect on execution score.

anova_result <- aov(Points ~ pressure_combined, data = pp_shots)
print("--- ANOVA RESULTS: EXECUTION VS PRESSURE ---")
print(summary(anova_result))

# 11. SAVE RESULTS --------------------------------------------------------
saveRDS(execution_summary, "execution_quality_summary.rds")



# ==============================================================================
# DAY 9A: SHOT SEQUENCE ANALYSIS (Armaan) - FIXED
# Objective: Identify common opening strategies (e.g., "Guard-Guard-Draw")
# ==============================================================================

# 12. CREATE SHOT SEQUENCES -----------------------------------------------
# We collapse the first 3 shots of every end into a single text string
shot_sequences <- pp_shots %>%
  # --- FIX: RE-CALCULATE SHOT ORDER HERE ---
  arrange(match_id, EndID, TeamID, ShotID) %>%
  group_by(match_id, EndID, TeamID) %>%
  mutate(shot_order = row_number()) %>%
  ungroup() %>%
  # -----------------------------------------
filter(shot_order <= 3) %>%
  group_by(match_id, EndID, TeamID, pressure_combined) %>%
  summarize(
    # Create the combo string (e.g. "Draw-Guard-Takeout")
    sequence = paste(shot_type_name, collapse = "-"),
    # Calculate how well they executed this sequence (Mean Points 0-4)
    avg_execution = mean(Points, na.rm = TRUE),
    .groups = "drop"
  )

# 13. ANALYZE COMMON SEQUENCES --------------------------------------------
# Count how often each sequence happens under different pressure
sequence_stats <- shot_sequences %>%
  group_by(pressure_combined, sequence) %>%
  summarize(
    frequency = n(),
    success_rate = mean(avg_execution, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(pressure_combined, desc(frequency))

# Print top 5 strategies for High Pressure vs Low Pressure
print("--- TOP STRATEGIES: LOW PRESSURE ---")
print(head(filter(sequence_stats, pressure_combined == "low"), 5))

print("--- TOP STRATEGIES: HIGH PRESSURE ---")
print(head(filter(sequence_stats, pressure_combined == "high"), 5))

print("--- TOP STRATEGIES: VERY HIGH PRESSURE ---")
print(head(filter(sequence_stats, pressure_combined == "very_high"), 5))

# 14. SAVE RESULTS --------------------------------------------------------
saveRDS(sequence_stats, "shot_sequences_summary.rds")
