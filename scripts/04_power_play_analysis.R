# scripts/04_power_play_analysis.R
# Day 8S: Core Analysis 1 - Scoring Under Pressure

library(tidyverse)
library(broom) # For tidy statistical test outputs

# 1. LOAD DATA ------------------------------------------------------------
# We use the final dataset created by Armaan on Day 7
pp_data <- readRDS("pp_analysis_ready.rds")

# --- EMERGENCY FIX: RE-APPLY DAY 7 LOGIC ---
pp_data <- pp_data %>%
  mutate(
    pressure_combined = case_when(
      pressure_situation == "high" & pressure_leverage == "high" ~ "very_high",
      pressure_situation == "high" | pressure_leverage == "high" ~ "high",
      pressure_situation == "medium" | pressure_leverage == "medium" ~ "medium",
      TRUE ~ "low"
    )
  )

# Set factor levels
pp_data$pressure_combined <- factor(pp_data$pressure_combined, 
                                    levels = c("low", "medium", "high", "very_high"))
# -------------------------------------------

# --- FIX: RECOVER MISSING SCORE DATA (CORRECTED) ---
# 1. Read the raw data
raw_ends <- read_csv("data/raw/Ends.csv")

# 2. Create matching ID and select the 'Result' column directly
# (The raw file already calls it 'Result', not 'Score')
raw_ends <- raw_ends %>%
  mutate(match_id = paste(CompetitionID, SessionID, GameID, sep = "_")) %>%
  select(match_id, EndID, TeamID, Result) 

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
