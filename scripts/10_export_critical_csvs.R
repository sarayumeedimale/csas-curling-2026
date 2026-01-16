# ==============================================================================
# 10_export_critical_csvs.R
# Purpose: Convert RDS files to CSV for final submission/upload
# ==============================================================================

library(tidyverse)
library(broom)

# Ensure output directory exists
if(!dir.exists("output/csv_data")) dir.create("output/csv_data", recursive = TRUE)

print("--- Starting CSV Export ---")

# 1. LOAD MAIN DATA
pp_data <- readRDS("pp_analysis_ready.rds")
team_profiles <- readRDS("team_profiles.rds")
pp_model <- readRDS("final_predictive_model.rds")
stones <- read_csv("data/raw/Stones.csv", show_col_types = FALSE)

# --- CSV #1: CLEAN POWER PLAY DATA ---
# This is the master dataset for the analysis
# (Ensure scores are recovered if missing, just like in script 08)
raw_ends <- read_csv("data/raw/Ends.csv", show_col_types = FALSE) %>%
  mutate(match_id = paste(CompetitionID, SessionID, GameID, sep = "_")) %>%
  select(match_id, EndID, TeamID, Result)

if("Result" %in% colnames(pp_data)) pp_data <- select(pp_data, -Result)
pp_data_clean <- pp_data %>%
  left_join(raw_ends, by = c("match_id", "EndID", "TeamID")) %>%
  filter(PowerPlay == 1)

write_csv(pp_data_clean, "output/csv_data/1_power_play_data.csv")
print("✓ 1. power_play_data.csv saved")

# ==============================================================================
# RESUME EXPORT FROM CSV #2 (FIXED TYPE MISMATCH)
# ==============================================================================

# --- CSV #2: TEAM RANKINGS (PPCS) ---
print("Generating CSV #2...")

# 1. Recalculate PPCS (Safety)
if(!"ppcs" %in% colnames(team_profiles)) {
  team_profiles <- team_profiles %>% mutate(ppcs = mean_points_high - mean_points_low)
}

# 2. Prepare Counts with Character ID
# Force TeamID to character to prevent join errors
team_counts <- pp_data_clean %>%
  mutate(TeamID = as.character(TeamID)) %>%
  count(TeamID, name = "ends_played_fresh")

# 3. Join and Select
team_rankings <- team_profiles %>%
  # Force join key to character
  mutate(join_key = as.character(if("NOC" %in% names(.)) NOC else TeamID)) %>%
  left_join(team_counts, by = c("join_key" = "TeamID")) %>%
  arrange(desc(ppcs)) %>%
  select(any_of(c("NOC", "TeamID", "ppcs", "mean_points_low", "mean_points_high")), 
         total_pp_ends = ends_played_fresh)

write_csv(team_rankings, "output/csv_data/2_team_rankings.csv")
print("✓ 2. team_rankings.csv saved")

# --- CSV #3: MODEL COEFFICIENTS ---
print("Generating CSV #3...")

model_coeffs <- tidy(pp_model, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(interpretation = ifelse(estimate > 1, "Increases Success", "Decreases Success"))

write_csv(model_coeffs, "output/csv_data/3_model_coefficients.csv")
print("✓ 3. model_coefficients.csv saved")

# --- CSV #4: SHOT EXECUTION DATA ---
print("Generating CSV #4...")

shot_data_export <- stones %>%
  mutate(
    match_id = paste(CompetitionID, SessionID, GameID, sep = "_"),
    shot_type = case_when(
      Task %in% c(1, 2, 4, 5) ~ "Guard/Setup",
      Task %in% c(0, 3, 10)   ~ "Draw/Offense",
      Task %in% c(6, 7, 8, 9) ~ "Takeout",
      TRUE ~ "Other"
    )
  ) %>%
  inner_join(pp_data_clean %>% select(match_id, EndID, TeamID, pressure_combined), 
             by = c("match_id", "EndID", "TeamID")) %>%
  select(match_id, EndID, TeamID, ShotID, shot_type, Points, pressure_combined)

write_csv(shot_data_export, "output/csv_data/4_shot_execution.csv")
print("✓ 4. shot_execution.csv saved")

# --- CSV #5: SCORING TRENDS SUMMARY ---
print("Generating CSV #5...")

scoring_summary <- pp_data_clean %>%
  group_by(pressure_combined) %>%
  summarize(
    ends_played = n(),
    avg_points = mean(Result, na.rm = TRUE),
    success_rate = mean(Result >= 2, na.rm = TRUE)
  )

write_csv(scoring_summary, "output/csv_data/5_scoring_trends.csv")
print("✓ 5. scoring_trends.csv saved")

print("--- ALL CSVs EXPORTED SUCCESSFULLY ---")