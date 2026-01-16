library(tidyverse)

# Load the data uploaded [cite: 126]
games <- read_csv("data/raw/Games.csv")
ends <- read_csv("data/raw/Ends.csv")
stones <- read_csv("data/raw/Stones.csv")

# Merge together 
master_data <- games %>%
  left_join(ends, by = c("CompetitionID", "SessionID", "GameID")) %>%
  left_join(stones, by = c("CompetitionID", "SessionID", "GameID", "EndID", "TeamID"))

# Create unique IDs
master_data <- master_data %>%
  mutate(match_id = paste(CompetitionID, SessionID, GameID, sep = "_"),
         end_id = paste(match_id, EndID, sep = "_"))

# -----------------------------------------------------------------------------
# Day 2S: Game state variables calculated
# -----------------------------------------------------------------------------

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

# STEP 2: Calculate Hammer Possession (CORRECTED FOR MIXED DOUBLES)
ends_clean <- ends %>%
  mutate(match_id = paste(CompetitionID, SessionID, GameID, sep = "_"))

hammer_start <- games %>%
  mutate(match_id = paste(CompetitionID, SessionID, GameID, sep = "_")) %>%
  select(match_id, TeamID1, TeamID2, LSFE)

hammer_logic <- ends_clean %>%
  left_join(ends_clean %>% select(match_id, EndID, TeamID, Result), 
            by = c("match_id", "EndID"), 
            suffix = c("", "_opp"),
            relationship = "many-to-many") %>%
  filter(TeamID != TeamID_opp) %>%
  left_join(hammer_start, by = "match_id") %>%
  group_by(match_id, TeamID) %>%
  arrange(EndID, .by_group = TRUE) %>%
  mutate(hammer_owner_raw = case_when(
    # 1. First End Logic
    EndID == 1 & LSFE == 1 ~ TeamID1,
    EndID == 1 & LSFE == 0 ~ TeamID2,
    
    # 2. Standard Scoring Logic (Scorer loses hammer)
    lag(Result, default=0) > 0 ~ TeamID_opp,
    lag(Result_opp, default=0) > 0 ~ TeamID,
    
    # 3. BLANK END LOGIC (Mixed Doubles Specific)
    # If the score was 0-0, the hammer SWAPS.
    lag(Result, default=0) == 0 & lag(Result_opp, default=0) == 0 ~ TeamID_opp,
    
    TRUE ~ NA_real_
  )) %>%
  fill(hammer_owner_raw, .direction = "down") %>%
  mutate(has_hammer = (TeamID == hammer_owner_raw), ends_remaining = 8 - EndID) %>%
  select(match_id, EndID, TeamID, has_hammer, ends_remaining)


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
# We strictly filter for 1 or 2 (as per data dictionary)
pp_data <- master_data %>%
  filter(PowerPlay %in% c(1, 2))

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


# -----------------------------------------------------------------------------
# DAY 3S: Exploratory analysis of power plays
# -----------------------------------------------------------------------------
pp_master <- pp_data
# 1. REMOVE ANOMALY (Match 0_10_1, End 7)
pp_master_clean <- pp_master %>%
  filter(has_hammer == TRUE)
print(paste("Cleaned dataset size:", nrow(pp_master_clean), "stones"))

# 2. PLOT 1: When do teams use power plays? (distribution by end)
# We use distinct() to count Ends, not stones
plot1 <- pp_master_clean %>%
  distinct(match_id, EndID) %>%
  ggplot(aes(x = factor(EndID))) +
  geom_bar(fill = "#2c3e50") +
  labs(title = "Power Play Usage by End",
       subtitle = "Teams save the Power Play for the late game (Ends 6-8)",
       x = "End Number", y = "Count of Power Plays") +
  theme_minimal()
print(plot1)

# 3. PLOT 2: REAL Usage Rate (Normalized by how often the score happens)
# Step A: Count how many times teams were EVER at these score diffs (Denominator)
total_situations <- master_data %>%
  distinct(match_id, EndID, TeamID, score_diff) %>%
  count(score_diff, name = "total_ends_played")

# Step B: Count how many times they used Power Play (Numerator)
pp_counts <- pp_master_clean %>%
  distinct(match_id, EndID, TeamID, score_diff) %>%
  count(score_diff, name = "pp_count")

# Step C: Plot the Percentage (Rate)
plot2 <- pp_counts %>%
  left_join(total_situations, by = "score_diff") %>%
  mutate(pp_rate = pp_count / total_ends_played) %>%
  # Filter out rare blowout scores (-6 or +6) to clean up the chart
  filter(total_ends_played > 20) %>% 
  ggplot(aes(x = score_diff, y = pp_rate)) +
  geom_col(fill = "#d62728", color = "white") + # Red for "Action"
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(-5, 5, 1)) +
  labs(title = "When Do Teams Pull the Trigger?",
       subtitle = "Power Play Usage Rate % by Score Differential",
       x = "Score Diff (Negative = Losing)", 
       y = "Usage Rate (%)") +
  theme_minimal()

print(plot2)

# 4. How often do teams with hammer use PP? (should be 100%)
print("Hammer Validation (After Clean):")
pp_master_clean %>%
  distinct(match_id, EndID, has_hammer) %>%
  count(has_hammer)