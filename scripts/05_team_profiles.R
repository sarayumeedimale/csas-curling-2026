# ==============================================================================
# DAY 10A & 11S: TEAM PROFILES & PPCS RANKING (FINAL)
# Authors: Armaan (Stats) & Sarayu (Ranking)
# Objective: Calculate PPCS and Rank Teams (removing invalid data)
# ==============================================================================

library(tidyverse)

# 1. LOAD DATA ------------------------------------------------------------
if (!exists("pp_data")) {
  if (file.exists("data/clean/pp_data.rds")) {
    pp_data <- readRDS("data/clean/pp_data.rds")
  } else {
    source("scripts/03_pressure_metrics.R") 
  }
}

competitors <- read_csv("data/raw/Competitors.csv", show_col_types = FALSE) %>%
  select(TeamID, NOC) %>%
  distinct()

if (!"NOC" %in% names(pp_data)) {
  pp_data <- pp_data %>% left_join(competitors, by = "TeamID")
}

# 2. CALCULATE TEAM STATISTICS --------------------------------------------
team_pp_stats <- pp_data %>%
  filter(PowerPlay == 1) %>%
  group_by(NOC) %>%
  summarize(
    n_pp = n(),
    n_pp_high_pressure = sum(pressure_combined %in% c("high", "very_high")),
    
    # Calculate Scoring Potential
    mean_points_low = mean(Result[pressure_combined == "low"], na.rm = TRUE),
    mean_points_high = mean(Result[pressure_combined %in% c("high", "very_high")], na.rm = TRUE)
  ) %>%
  # Filter 1: Minimum sample size
  filter(n_pp >= 5) %>%
  # Filter 2: Must have played in BOTH Low and High pressure (Remove NaNs)
  filter(!is.nan(mean_points_low) & !is.nan(mean_points_high)) %>%
  
  # 3. CALCULATE PPCS (CLUTCH SCORE) --------------------------------------
mutate(
  pp_clutch_score = mean_points_high - mean_points_low,
  clutch_category = case_when(
    pp_clutch_score > 0.5 ~ "Ice Cold Killer (Better under pressure)",
    pp_clutch_score > 0 ~ "Resilient (Steady)",
    TRUE ~ "Crumbles (Worse under pressure)"
  )
) %>%
  arrange(desc(pp_clutch_score))

# 4. IDENTIFY WINNERS & LOSERS --------------------------------------------
# Top 3 "Clutch" Teams
top_3_clutch <- team_pp_stats %>%
  slice_head(n = 3) %>%
  select(NOC, n_pp, low_avg=mean_points_low, high_avg=mean_points_high, PPCS=pp_clutch_score)

# Bottom 3 "Chokers"
bottom_3_chokers <- team_pp_stats %>%
  slice_tail(n = 3) %>%
  arrange(pp_clutch_score) %>% 
  select(NOC, n_pp, low_avg=mean_points_low, high_avg=mean_points_high, PPCS=pp_clutch_score)

# 5. PRINT REPORT ---------------------------------------------------------
print("--- 🏆 THE CLUTCH PODIUM (Top 3) ---")
print(top_3_clutch)

print("--- 📉 THE CHOKE ZONE (Bottom 3) ---")
print(bottom_3_chokers)

saveRDS(team_pp_stats, "team_profiles.rds")