# ==============================================================================
# REPORT STATISTICS GENERATOR (UPDATED WITH WPA ANALYSIS)
# ==============================================================================

library(tidyverse)

pp_data <- readRDS("pp_analysis_ready.rds")
team_profiles <- readRDS("team_profiles.rds")
team_wpa <- readRDS("team_wpa_analysis.rds")
comparison <- readRDS("ppcs_wpa_comparison.rds")

pp_only <- pp_data %>% filter(PowerPlay == 1)

# ==============================================================================
# DATA OVERVIEW
# ==============================================================================

print("--- DATA OVERVIEW ---")
print(paste("Total power play ends:", nrow(pp_only)))
print(paste("Teams analyzed:", nrow(team_profiles)))

# ==============================================================================
# SCORING BY PRESSURE
# ==============================================================================

print("--- SCORING BY PRESSURE ---")
scoring <- pp_only %>%
  group_by(pressure_combined) %>%
  summarize(
    n = n(),
    mean_pts = round(mean(Result, na.rm = TRUE), 2),
    success_rate = round(mean(Result >= 2, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )
print(scoring)

low <- scoring %>% filter(pressure_combined == "low") %>% pull(mean_pts)
high <- scoring %>% filter(pressure_combined == "high") %>% pull(mean_pts)
very_high <- scoring %>% filter(pressure_combined == "very_high") %>% pull(mean_pts)
change <- round((very_high - low) / low * 100, 1)

print(paste("Low pressure avg:", low, "pts"))
print(paste("High pressure avg:", high, "pts"))
print(paste("Very high pressure avg:", very_high, "pts"))
print(paste("Change low to very high:", change, "%"))

# ==============================================================================
# PPCS RANKINGS
# ==============================================================================

print("--- PPCS RANKINGS ---")

print("TOP 3 CLUTCH:")
top3 <- team_profiles %>% slice_head(n = 3)
print(top3 %>% select(NOC, ppcs))

print("BOTTOM 3 CHOKERS:")
bot3 <- team_profiles %>% slice_tail(n = 3) %>% arrange(ppcs)
print(bot3 %>% select(NOC, ppcs))

print(paste("Teams better under pressure:", sum(team_profiles$ppcs > 0)))
print(paste("Teams worse under pressure:", sum(team_profiles$ppcs < 0)))

# ==============================================================================
# MODEL STATS
# ==============================================================================

if(file.exists("model_evaluation.rds")) {
  model_eval <- readRDS("model_evaluation.rds")
  print("--- MODEL STATS ---")
  print(paste("Accuracy:", round(model_eval$accuracy * 100, 1), "%"))
  print(paste("AUC:", round(model_eval$auc, 3)))
}

# ==============================================================================
# WPA ANALYSIS (NEW)
# ==============================================================================

print("--- WPA ANALYSIS ---")

print("TOP 3 BY WIN PROBABILITY IMPACT:")
top_wpa <- team_wpa %>% arrange(desc(avg_wpa)) %>% head(3)
print(top_wpa %>% select(NOC, n_pp, avg_wpa))

print("BOTTOM 3 BY WIN PROBABILITY IMPACT:")
bot_wpa <- team_wpa %>% arrange(avg_wpa) %>% head(3)
print(bot_wpa %>% select(NOC, n_pp, avg_wpa))

cor_value <- cor(comparison$ppcs, comparison$avg_wpa, use = "complete.obs")
print(paste("PPCS vs WPA correlation:", round(cor_value, 3)))

# ==============================================================================
# TRUE CLUTCH TEAMS (HIGH PPCS + HIGH WPA)
# ==============================================================================

print("--- TRUE CLUTCH TEAMS (High PPCS AND High WPA) ---")
true_clutch <- comparison %>% 
  filter(ppcs > 0, avg_wpa > 0) %>% 
  select(NOC, ppcs, avg_wpa) %>% 
  arrange(desc(ppcs))
print(true_clutch)

print("--- FALSE CLUTCH (High PPCS but Negative WPA) ---")
false_clutch <- comparison %>% 
  filter(ppcs > 0, avg_wpa < 0) %>% 
  select(NOC, ppcs, avg_wpa)
if(nrow(false_clutch) > 0) {
  print(false_clutch)
} else {
  print("None")
}

print("--- UNDERPERFORMERS (Negative PPCS and Negative WPA) ---")
underperformers <- comparison %>%
  filter(ppcs < 0, avg_wpa < 0) %>%
  select(NOC, ppcs, avg_wpa) %>%
  arrange(ppcs)
print(underperformers)

# ==============================================================================
# PRESSURE RESPONSE SUMMARY
# ==============================================================================

print("--- PRESSURE RESPONSE PATTERNS ---")

pp_with_noc <- pp_only
if(!"NOC" %in% colnames(pp_with_noc)) {
  competitors <- read_csv("data/raw/Competitors.csv", show_col_types = FALSE) %>%
    select(TeamID, NOC) %>% distinct() %>% mutate(TeamID = as.numeric(TeamID))
  pp_with_noc <- pp_only %>%
    mutate(TeamID = as.numeric(TeamID)) %>%
    left_join(competitors, by = "TeamID")
}

pressure_slopes <- pp_with_noc %>%
  filter(!is.na(NOC)) %>%
  mutate(pressure_num = case_when(
    pressure_combined == "low" ~ 1,
    pressure_combined == "medium" ~ 2,
    pressure_combined == "high" ~ 3,
    pressure_combined == "very_high" ~ 4
  )) %>%
  group_by(NOC) %>%
  filter(n() >= 5) %>%
  summarize(
    slope = cor(pressure_num, Result, use = "complete.obs"),
    .groups = "drop"
  ) %>%
  filter(!is.na(slope)) %>%
  arrange(desc(slope))

print("RISERS (Performance increases with pressure):")
print(head(pressure_slopes, 5))

print("FOLDERS (Performance decreases with pressure):")
print(tail(pressure_slopes, 5))

print("--- DONE ---")
