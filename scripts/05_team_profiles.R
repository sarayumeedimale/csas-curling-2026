# ==============================================================================
# DAY 11: TEAM RANKINGS & SELECTION (FINAL FIX)
# ==============================================================================

library(tidyverse)

# 1. LOAD DATA & RECOVER SCORES -------------------------------------------
pp_data <- readRDS("pp_analysis_ready.rds")

# Load Raw Ends for Scores
raw_ends <- read_csv("data/raw/Ends.csv", show_col_types = FALSE) %>%
  mutate(match_id = paste(CompetitionID, SessionID, GameID, sep = "_")) %>%
  select(match_id, EndID, TeamID, Result)

# Load Team Names (To fix the "CAN/SWE" selection issue)
# We assume the file is named 'Teams.csv' in data/raw/
raw_teams <- read_csv("data/raw/Teams.csv", show_col_types = FALSE) %>%
  select(TeamID, NOC) %>% # Select only ID and Country Code
  distinct()

# Join Scores
if("Result" %in% colnames(pp_data)) pp_data <- select(pp_data, -Result)

pp_data <- pp_data %>%
  left_join(raw_ends, by = c("match_id", "EndID", "TeamID")) %>%
  # --- CRITICAL FIX: DATA CLEANING ---
  # Remove impossible scores (9, 10, etc represent X/Conceded)
  filter(Result <= 8) %>%
  # Join Team Names
  left_join(raw_teams, by = "TeamID")

print("Data Loaded, Scores Cleaned, and Names Joined.")

# 2. CALCULATE CLUTCH RANKINGS --------------------------------------------
team_pp_stats <- pp_data %>%
  filter(PowerPlay == 1) %>%
  group_by(NOC) %>%  # Now we can group by Country Code!
  summarize(
    n_pp = n(),
    n_pp_high = sum(pressure_combined %in% c("high", "very_high")),
    avg_low  = mean(Result[pressure_combined %in% c("low", "medium")], na.rm=TRUE),
    avg_high = mean(Result[pressure_combined %in% c("high", "very_high")], na.rm=TRUE)
  ) %>%
  filter(n_pp >= 5) %>%
  filter(!is.nan(avg_low) & !is.nan(avg_high)) %>%
  mutate(
    ppcs = avg_high - avg_low,
    category = case_when(
      ppcs > 0.5 ~ "Clutch God",
      ppcs > 0   ~ "Resilient",
      TRUE       ~ "Choker"
    )
  ) %>%
  arrange(desc(ppcs))

print("--- FINAL RANKINGS (Top 5) ---")
print(head(team_pp_stats, 5))

# 3. SELECT CASE STUDIES --------------------------------------------------
# 1. The Hero (Top of list)
clutch_team <- team_pp_stats %>% slice(1) %>% pull(NOC)

# 2. The Villain (Bottom of list)
choke_team <- team_pp_stats %>% slice(n()) %>% pull(NOC)

# 3. The Contender (Specific Big Nations)
contender_team <- team_pp_stats %>%
  filter(!NOC %in% c(clutch_team, choke_team)) %>%
  filter(n_pp >= 10) %>%
  filter(NOC %in% c("CAN", "SWE", "GBR", "USA", "NOR", "SUI", "ITA", "SCO")) %>%
  slice(1) %>%
  pull(NOC)

print(paste("Clutch Hero:", clutch_team))
print(paste("Choke Villain:", choke_team))
print(paste("Olympic Contender:", contender_team))

# 4. FIND HIGHLIGHT MOMENTS -----------------------------------------------
# We look for the best/worst moments for these specific teams
highlights <- pp_data %>%
  filter(PowerPlay == 1) %>%
  filter(NOC %in% c(clutch_team, choke_team)) %>%
  filter(pressure_combined %in% c("high", "very_high")) %>%
  group_by(NOC) %>%
  # Get best result for Clutch team, worst for Choke team
  filter(Result == if_else(NOC == clutch_team, max(Result), min(Result))) %>%
  slice(1) %>% # Take just one example per team
  select(NOC, match_id, EndID, Result, pressure_combined)

print("--- HIGHLIGHT MOMENTS ---")
print(highlights)

# 5. SAVE EVERYTHING ------------------------------------------------------
saveRDS(team_pp_stats, "team_clutch_rankings.rds")
saveRDS(highlights, "case_study_moments.rds")