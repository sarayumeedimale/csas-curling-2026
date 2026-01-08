# ==============================================================================
# DAY 10A: TEAM PROFILES & SCOUTING (Armaan)
# Objective: Calculate team-level performance metrics (The "Clutch Score")
# ==============================================================================

library(tidyverse)

# 1. LOAD DATA ------------------------------------------------------------
# We need the 'pp_data' (Ends with Pressure) and 'Competitors' (Team Names)

# Check if pp_data exists; if not, try to load it or source the metric script
if (!exists("pp_data")) {
  if (file.exists("data/clean/pp_data.rds")) {
    pp_data <- readRDS("data/clean/pp_data.rds")
  } else {
    print("Regenerating Pressure Metrics (this may take a moment)...")
    source("scripts/03_pressure_metrics.R") 
  }
}

# Load Competitors to get country codes (NOC)
competitors <- read_csv("data/raw/Competitors.csv", show_col_types = FALSE) %>%
  select(TeamID, NOC) %>%
  distinct()

# Ensure pp_data has NOC info
if (!"NOC" %in% names(pp_data)) {
  pp_data <- pp_data %>%
    left_join(competitors, by = "TeamID")
}

# 2. CALCULATE TEAM STATISTICS --------------------------------------------
team_pp_stats <- pp_data %>%
  # Filter for Power Play ends only
  filter(PowerPlay == 1) %>%
  group_by(NOC) %>%
  summarize(
    n_pp = n(),
    n_pp_high_pressure = sum(pressure_combined %in% c("high", "very_high")),
    
    # Calculate Scoring Potential
    mean_points_low = mean(Result[pressure_combined == "low"], na.rm = TRUE),
    mean_points_high = mean(Result[pressure_combined %in% c("high", "very_high")], na.rm = TRUE)
  ) %>%
  # Filter for sample size (at least 5 power plays total)
  filter(n_pp >= 5) %>%
  
  # 3. CREATE THE "CLUTCH SCORE" (PPCS) -----------------------------------
# PPCS = Difference between High Pressure scoring and Low Pressure scoring
mutate(
  pp_clutch_score = mean_points_high - mean_points_low,
  clutch_category = case_when(
    pp_clutch_score > 0.5 ~ "Ice Cold Killer (Better under pressure)",
    pp_clutch_score > 0 ~ "Resilient (Steady)",
    TRUE ~ "Crumbles (Worse under pressure)"
  )
) %>%
  arrange(desc(pp_clutch_score))

# 4. VIEW & SAVE RESULTS --------------------------------------------------
print("--- TEAM SCOUTING REPORT: TOP CLUTCH PERFORMERS ---")
print(head(team_pp_stats, 10))

# Save for the Visualization day
saveRDS(team_pp_stats, "team_profiles.rds")