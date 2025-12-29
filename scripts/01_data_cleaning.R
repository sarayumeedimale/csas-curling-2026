library(tidyverse)

# Load the data Sarayu uploaded [cite: 126]
games <- read_csv("data/raw/Games.csv")
ends <- read_csv("data/raw/Ends.csv")
stones <- read_csv("data/raw/Stones.csv")

# Merge them together 
master_data <- games %>%
  left_join(ends, by = c("CompetitionID", "SessionID", "GameID")) %>%
  left_join(stones, by = c("CompetitionID", "SessionID", "GameID", "EndID"))

# Create the unique IDs your plan requires 
master_data <- master_data %>%
  mutate(match_id = paste(CompetitionID, SessionID, GameID, sep = "_"),
         end_id = paste(match_id, EndID, sep = "_"))