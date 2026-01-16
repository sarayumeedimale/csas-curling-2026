# ==============================================================================
# ADVANCED ANALYSIS: WPA Impact + Pressure Response Curves
# ==============================================================================

library(tidyverse)

# 1. LOAD DATA
pp_data <- readRDS("pp_analysis_ready.rds")
team_profiles <- readRDS("team_profiles.rds")
competitors <- read_csv("data/raw/Competitors.csv", show_col_types = FALSE) %>%
  select(TeamID, NOC) %>% 
  distinct() %>%
  mutate(TeamID = as.numeric(TeamID))

# Check if NOC already exists, if not add it
pp_only <- pp_data %>% 
  filter(PowerPlay == 1) %>%
  mutate(TeamID = as.numeric(TeamID))

if(!"NOC" %in% colnames(pp_only)) {
  pp_only <- pp_only %>% left_join(competitors, by = "TeamID")
}

# Verify NOC exists now
print("Columns in pp_only:")
print(colnames(pp_only))
print(paste("NOC column exists:", "NOC" %in% colnames(pp_only)))

# ==============================================================================
# PART A: WIN PROBABILITY ADDED (WPA) ANALYSIS
# ==============================================================================

team_wpa <- pp_only %>%
  filter(!is.na(NOC)) %>%
  group_by(NOC) %>%
  summarize(
    n_pp = n(),
    total_wpa = sum(wpa, na.rm = TRUE),
    avg_wpa = mean(wpa, na.rm = TRUE),
    avg_wpa_high_pressure = mean(wpa[pressure_combined %in% c("high", "very_high")], na.rm = TRUE),
    avg_wpa_low_pressure = mean(wpa[pressure_combined == "low"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_pp >= 5) %>%
  mutate(ppis = avg_wpa_high_pressure - avg_wpa_low_pressure) %>%
  arrange(desc(avg_wpa))

# Compare PPCS vs WPA
comparison <- team_profiles %>%
  select(NOC, ppcs) %>%
  left_join(team_wpa %>% select(NOC, avg_wpa, ppis), by = "NOC") %>%
  filter(!is.na(avg_wpa)) %>%
  mutate(
    ppcs_rank = rank(-ppcs),
    wpa_rank = rank(-avg_wpa)
  )

cor_value <- cor(comparison$ppcs, comparison$avg_wpa, use = "complete.obs")

saveRDS(team_wpa, "team_wpa_analysis.rds")
saveRDS(comparison, "ppcs_wpa_comparison.rds")

# ==============================================================================
# PART B: PRESSURE RESPONSE CURVES
# ==============================================================================

pp_only <- pp_only %>%
  mutate(
    pressure_continuous = case_when(
      pressure_combined == "low" ~ 0.125,
      pressure_combined == "medium" ~ 0.375,
      pressure_combined == "high" ~ 0.625,
      pressure_combined == "very_high" ~ 0.875
    )
  )

pressure_curves <- pp_only %>%
  filter(!is.na(NOC)) %>%
  group_by(NOC, pressure_combined, pressure_continuous) %>%
  summarize(
    n = n(),
    mean_pts = mean(Result, na.rm = TRUE),
    mean_wpa = mean(wpa, na.rm = TRUE),
    .groups = "drop"
  )

teams_with_range <- pressure_curves %>%
  group_by(NOC) %>%
  filter(n() >= 3) %>%
  ungroup()

highlight_teams <- c("NZL", "EST", "CAN", "ITA", "USA", "SWE")


print("--- TOP 5 WPA ---")
print(head(team_wpa %>% select(NOC, n_pp, avg_wpa), 5))

print("--- BOTTOM 5 WPA ---")
print(tail(team_wpa %>% select(NOC, n_pp, avg_wpa), 5))

print(paste("PPCS vs WPA correlation:", round(cor_value, 3)))

print("--- TRUE CLUTCH (High PPCS + High WPA) ---")
print(comparison %>% filter(ppcs > 0, avg_wpa > 0) %>% select(NOC, ppcs, avg_wpa) %>% arrange(desc(ppcs)))

print("--- DONE ---")
