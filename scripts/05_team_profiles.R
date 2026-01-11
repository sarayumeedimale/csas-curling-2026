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


# ==============================================================================
# ARMAAN - DAY 11A: TEAM CASE STUDY SELECTION
# ==============================================================================

# 1. LOAD DATA (Ensuring we have Sarayu's latest files)
# Note: Adjust path if Sarayu saved it elsewhere, but this matches the plan
team_profiles <- readRDS("output/team_profiles.rds")
pp_data <- readRDS("output/pp_analysis_ready.rds")

# 2. IDENTIFY THE CHARACTERS
# We need 3 teams: The Hero (Clutch), The Villain (Choker), and The Contender

# CLUTCH TEAM: Highest PPCS (performs best under pressure)
clutch_team <- team_profiles %>%
  arrange(desc(ppcs_raw)) %>%
  slice(1) %>%
  pull(NOC)

# CHOKE TEAM: Lowest PPCS (performs worst under pressure)
choke_team <- team_profiles %>%
  arrange(ppcs_raw) %>%
  slice(1) %>%
  pull(NOC)

# CONTENDER: A major nation (CAN, SWE, GBR, USA, etc.) with lots of data
# who isn't already the clutch or choke team.
contender_team <- team_profiles %>%
  filter(!NOC %in% c(clutch_team, choke_team)) %>%
  filter(n_pp > 15) %>%  # Filter for high sample size
  filter(NOC %in% c("CAN", "SWE", "GBR", "USA", "NOR", "SUI", "ITA")) %>%
  arrange(desc(n_pp)) %>%
  slice(1) %>%
  pull(NOC)

# Print them out so you can see who they are in the Console
print(paste("Clutch Hero:", clutch_team))
print(paste("Choke Villain:", choke_team))
print(paste("Olympic Contender:", contender_team))

# 3. SAVE THE SELECTION
# We save these names so we can use them in the visualizations later
selected_teams <- c(clutch = clutch_team, choke = choke_team, contender = contender_team)
saveRDS(selected_teams, "output/selected_case_study_teams.rds")

# 4. FIND "HIGHLIGHT REEL" MOMENTS (For Figure 7)
# We need specific Match IDs to plot later.

# Find a game where the CLUTCH team scored 3+ points under HIGH pressure
clutch_moment <- pp_data %>%
  filter(NOC == clutch_team, pressure_combined == "high", Result >= 3) %>%
  arrange(desc(Result)) %>%
  slice(1) %>%
  select(match_id, EndID, NOC, Result, pressure_combined)

# Find a game where the CHOKE team scored 0 points under HIGH pressure
choke_moment <- pp_data %>%
  filter(NOC == choke_team, pressure_combined == "high", Result == 0) %>%
  slice(1) %>%
  select(match_id, EndID, NOC, Result, pressure_combined)

print("--- CLUTCH MOMENT TO VISUALIZE ---")
print(clutch_moment)

print("--- CHOKE MOMENT TO VISUALIZE ---")
print(choke_moment)

# Save these specific moments for the visualization script
case_study_moments <- bind_rows(clutch_moment, choke_moment)
saveRDS(case_study_moments, "output/case_study_moments.rds")


