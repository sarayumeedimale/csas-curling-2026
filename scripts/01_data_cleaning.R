library(tidyverse)

# Load the data uploaded [cite: 126]
games <- read_csv("data/raw/Games.csv")
ends <- read_csv("data/raw/Ends.csv")
stones <- read_csv("data/raw/Stones.csv")
View(ends)

# Merge together 
master_data <- games %>%
  left_join(ends, by = c("CompetitionID", "SessionID", "GameID")) %>%
  left_join(stones, by = c("CompetitionID", "SessionID", "GameID", "EndID", "TeamID"))

# Create unique IDs
master_data <- master_data %>%
  mutate(match_id = paste(CompetitionID, SessionID, GameID, sep = "_"),
         end_id = paste(match_id, EndID, sep = "_"))


# Day 2S: Game state variables calculated

# STEP 1: Calculate: game state at START of each end
  # temporary table from Ends data
  score_calculation <- ends %>%
    mutate(match_id = paste(CompetitionID, SessionID, GameID, sep = "_")) %>%
    group_by(match_id, TeamID) %>%
    arrange(match_id, EndID) %>%
  # calc: score before current end starts ("lag" takes score from previous row, cumsum() adds them all as a running total)
    mutate(my_score_before = cumsum(lag(Result, default = 0))) %>%
    ungroup()
# calc: Score Differential
  game_state_final <- score_calculation %>%
    left_join(score_calculation, 
              by = c("match_id", "EndID"), 
              suffix = c("", "_opp"),
              relationship = "many-to-many") %>%
    # Filter to see opponent's data
    filter(TeamID != TeamID_opp) %>%
    # Calculate difference
    mutate(score_diff = my_score_before - my_score_before_opp) %>%
    select(match_id, EndID, TeamID, my_score_before, score_diff)
head(game_state_final) ## interpretation: Before End 1 both teams starts with 0-0. Team 20 leads 1-0 entering End 2, then Team 22 scores 3, score_diff shows 1-3 deficit.

# STEP 2: Calculate Hammer Possession
ends_clean <- ends %>%
  mutate(match_id = paste(CompetitionID, SessionID, GameID, sep = "_"))
  # LSFE = 1 means TeamID1 has hammer. LSFE = 0 means TeamID2 has hammer.
  hammer_start <- games %>%
    mutate(match_id = paste(CompetitionID, SessionID, GameID, sep = "_")) %>%
    select(match_id, TeamID1, TeamID2, LSFE)
  # Apply logic to calculate hammer for all ends
  hammer_logic <- ends_clean %>%
    left_join(ends_clean %>% select(match_id, EndID, TeamID, Result), 
              by = c("match_id", "EndID"), 
              suffix = c("", "_opp"),
              relationship = "many-to-many") %>%
    filter(TeamID != TeamID_opp) %>%
  # Join with starting info so we know who started with it
  left_join(hammer_start, by = "match_id") %>%
  # Group by match (so we look at ends in order for THAT game only)
  group_by(match_id, TeamID) %>%
  arrange(EndID, .by_group = TRUE) %>%
  # Determine who "Should" have the hammer in this specific end
  mutate(hammer_owner_raw = case_when(
      # SCENARIO A: First End; If LSFE matches the row's TeamID, they have it.
      EndID == 1 & LSFE == 1 ~ TeamID1,
      EndID == 1 & LSFE == 0 ~ TeamID2,
      # SCENARIO B: End 2 or later
      lag(Result, default=0) > 0 ~ TeamID_opp,
      lag(Result_opp, default=0) > 0 ~ TeamID,
     # Blank End (0-0)
      TRUE ~ NA_real_)) %>%
  # If it was a blank end (NA), 'fill' copies value from row above to correctly keep the hammer with whoever had it last.
  fill(hammer_owner_raw, .direction = "down") %>%
    mutate(has_hammer = (TeamID == hammer_owner_raw),ends_remaining = 8 - EndID) %>%
    select(match_id, EndID, TeamID, has_hammer, ends_remaining)
head(hammer_logic) # Tracks turn-by-turn hammer possession, (score -> lose hammer) & (blank ends -> keep hammer).


# -----------------------------------------------------------------------------
# DAY 2A (Armaan): Merge Sarayu's Work & Clean Data
# -----------------------------------------------------------------------------

# 1. Join Sarayu's calculations back to the Master Data
# Right now, 'score_diff' and 'has_hammer' are in separate tables. We need to add them to master_data.
master_data <- master_data %>%
  left_join(game_state_final, by = c("match_id", "EndID", "TeamID")) %>%
  left_join(hammer_logic, by = c("match_id", "EndID", "TeamID"))

# [cite_start]2. Handle "Sentinel" Values in Stone Coordinates [cite: 167-168]
# The sensor records 4095 or 0 when the stone is invalid/not thrown.
# We must turn these into NA so they don't mess up our math.
# Clean ALL stone coordinate columns at once
master_data <- master_data %>%
  mutate(across(starts_with("stone_"), ~ ifelse(. == 4095 | . == 0, NA, .)))

# [cite_start]3. Create the Power Play Subset [cite: 170-173]
# We only care about ends where the PowerPlay flag is active.
pp_data <- master_data %>%
  filter(!is.na(PowerPlay))

# 4. Save the clean data for the next phase
# We save it as an .rds file because it keeps all our variable types safe.
write_rds(pp_data, "data/pp_master.rds")

print(paste("Day 2A Complete: Power Play subset saved with", nrow(pp_data), "rows."))

# Pick 5 random games to inspect
set.seed(2026) # Keeps the "random" games the same every time
random_match_ids <- sample(unique(pp_data$match_id), 5)

# Show the hammer flow for these 5 games
pp_data %>%
  filter(match_id %in% random_match_ids) %>%
  select(match_id, EndID, TeamID, Result, has_hammer, score_diff) %>%
  arrange(match_id, EndID) %>%
  print(n = 50)

