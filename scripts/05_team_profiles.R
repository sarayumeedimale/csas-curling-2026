# ==============================================================================
# DAY 10A & 11S: TEAM PROFILES & PPCS RANKING
# ==============================================================================

library(tidyverse)

# 1. LOAD DATA
pp_data <- readRDS("pp_analysis_ready.rds")

# 2. CALCULATE TEAM STATISTICS
team_pp_stats <- pp_data %>%
  filter(PowerPlay == 1) %>%
  group_by(NOC) %>%
  summarize(
    n_pp = n(),
    n_pp_high_pressure = sum(pressure_combined %in% c("high", "very_high")),
    mean_points_low = mean(Result[pressure_combined == "low"], na.rm = TRUE),
    mean_points_high = mean(Result[pressure_combined %in% c("high", "very_high")], na.rm = TRUE)
  ) %>%
  filter(n_pp >= 5) %>%
  filter(!is.nan(mean_points_low) & !is.nan(mean_points_high)) %>%
  mutate(
    ppcs = mean_points_high - mean_points_low,
    clutch_category = case_when(
      ppcs > 0.5 ~ "Clutch",
      ppcs > 0 ~ "Resilient",
      TRUE ~ "Chokes"
    )
  ) %>%
  arrange(desc(ppcs))

# 3. PRINT RESULTS
print("--- TOP 3 CLUTCH ---")
print(head(team_pp_stats, 3))

print("--- BOTTOM 3 CHOKERS ---")
print(tail(team_pp_stats, 3))

saveRDS(team_pp_stats, "team_profiles.rds")

# ==============================================================================
# DAY 11A: CASE STUDY SELECTION
# ==============================================================================

clutch_team <- team_pp_stats %>% slice(1) %>% pull(NOC)
choke_team <- team_pp_stats %>% slice(n()) %>% pull(NOC)

contender_team <- team_pp_stats %>%
  filter(!NOC %in% c(clutch_team, choke_team)) %>%
  filter(n_pp >= 10) %>%
  filter(NOC %in% c("CAN", "SWE", "GBR", "USA", "NOR", "SUI", "ITA")) %>%
  slice(1) %>%
  pull(NOC)

print(paste("Clutch Hero:", clutch_team))
print(paste("Choke Villain:", choke_team))
print(paste("Olympic Contender:", contender_team))

selected_teams <- c(clutch = clutch_team, choke = choke_team, contender = contender_team)
saveRDS(selected_teams, "selected_case_study_teams.rds")

# FIND HIGHLIGHT MOMENTS
pp_only <- pp_data %>% filter(PowerPlay == 1)

clutch_moment <- pp_only %>%
  filter(NOC == clutch_team, pressure_combined %in% c("high", "very_high"), Result >= 2) %>%
  arrange(desc(Result)) %>%
  slice(1) %>%
  select(match_id, EndID, NOC, Result, pressure_combined)

choke_moment <- pp_only %>%
  filter(NOC == choke_team, pressure_combined %in% c("high", "very_high"), Result <= 1) %>%
  arrange(Result) %>%
  slice(1) %>%
  select(match_id, EndID, NOC, Result, pressure_combined)

print("--- CLUTCH MOMENT ---")
print(clutch_moment)

print("--- CHOKE MOMENT ---")
print(choke_moment)

case_study_moments <- bind_rows(
  clutch_moment %>% mutate(type = "clutch"),
  choke_moment %>% mutate(type = "choke")
)
saveRDS(case_study_moments, "case_study_moments.rds")

