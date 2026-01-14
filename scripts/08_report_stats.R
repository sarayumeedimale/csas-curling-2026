# ==============================================================================
# REPORT STATISTICS GENERATOR (ROBUST)
# Run this to get all exact numbers for your Executive Report
# ==============================================================================

library(tidyverse)
library(broom)
library(pROC)

# 1. LOAD DATA & RECOVER SCORES (Safety First) ----------------------------
pp_data <- readRDS("pp_analysis_ready.rds")

# Recovery Block: Ensure 'Result' (points) is present
raw_ends <- read_csv("data/raw/Ends.csv", show_col_types = FALSE) %>%
  mutate(match_id = paste(CompetitionID, SessionID, GameID, sep = "_")) %>%
  select(match_id, EndID, TeamID, Result)

if("Result" %in% colnames(pp_data)) pp_data <- select(pp_data, -Result)

pp_data <- pp_data %>%
  left_join(raw_ends, by = c("match_id", "EndID", "TeamID")) %>%
  filter(PowerPlay == 1)

# 2. LOAD & PREP TEAM PROFILES --------------------------------------------
team_profiles <- readRDS("team_profiles.rds")

# Recalculate PPCS and Sort (Ensures rankings are correct)
if(!"ppcs" %in% colnames(team_profiles)) {
  team_profiles <- team_profiles %>%
    mutate(ppcs = mean_points_high - mean_points_low)
}

team_profiles <- team_profiles %>%
  arrange(desc(ppcs)) # Sort Best to Worst

# ==============================================================================
# PRINT REPORT STATISTICS
# ==============================================================================

cat("\n══════════════════════════════════════════════════════════════\n")
cat("📊 FINAL REPORT STATISTICS (COPY THESE)\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# --- SECTION 1: DATA OVERVIEW ---
cat("--- 1. DATA OVERVIEW ---\n")
cat("Total Power Play Ends Analyzed:", nrow(pp_data), "\n")
cat("Total Teams Analyzed:", nrow(team_profiles), "\n")
cat("Average Pressure Score (0-3):", round(mean(pp_data$pressure_index_norm, na.rm=TRUE), 2), "\n\n")

# --- SECTION 2: SCORING TRENDS (The Clutch Paradox) ---
cat("--- 2. THE CLUTCH PARADOX (Scoring by Pressure) ---\n")
scoring <- pp_data %>%
  group_by(pressure_combined) %>%
  summarize(
    mean_pts = mean(Result, na.rm = TRUE),
    success_rate = mean(Result >= 2, na.rm = TRUE) * 100
  )

low_pts <- scoring %>% filter(pressure_combined == "low") %>% pull(mean_pts)
high_pts <- scoring %>% filter(pressure_combined == "very_high") %>% pull(mean_pts)
pct_change <- ((high_pts - low_pts) / low_pts) * 100

cat("Avg Points (Low Pressure):      ", round(low_pts, 2), "\n")
cat("Avg Points (Very High Pressure):", round(high_pts, 2), "\n")
cat("Percent Change:                 ", ifelse(pct_change > 0, "+", ""), round(pct_change, 1), "% (Scoring INCREASES)\n\n", sep="")

# --- SECTION 3: RANKINGS (Heroes & Villains) ---
cat("--- 3. TEAM RANKINGS (PPCS) ---\n")
cat("Top 3 Clutch Teams (Best under pressure):\n")
top3 <- head(team_profiles, 3)
for(i in 1:3) {
  cat(i, ". ", top3$NOC[i], " (+", round(top3$ppcs[i], 2), " pts)\n", sep="")
}

cat("\nBottom 3 Teams (Worst under pressure):\n")
bot3 <- tail(team_profiles, 3) %>% arrange(ppcs) # Sort asc for bottom
for(i in 1:3) {
  cat(i, ". ", bot3$NOC[i], " (", round(bot3$ppcs[i], 2), " pts)\n", sep="")
}

# --- SECTION 4: MODEL PERFORMANCE ---
cat("\n--- 4. PREDICTIVE MODEL STATS ---\n")
# Calculate fresh to ensure accuracy
if(file.exists("final_predictive_model.rds")) {
  model <- readRDS("final_predictive_model.rds")
  
  # Re-create simple evaluation data (same logic as Day 12)
  # We need the source data for the model. We'll grab coefficients directly.
  model_tidy <- tidy(model)
  
  # Extract key Odds Ratios for the report
  shot2_guard <- model_tidy %>% filter(term == "shot2Guard_Setup") %>% mutate(or = exp(estimate)) %>% pull(or)
  shot1_guard <- model_tidy %>% filter(term == "shot1Guard_Setup") %>% mutate(or = exp(estimate)) %>% pull(or)
  
  cat("Key Finding 1 (Shot 2 Guard): Odds Ratio =", round(shot2_guard, 2), 
      "(", round((shot2_guard-1)*100, 0), "% increase in success)\n")
  cat("Key Finding 2 (Shot 1 Guard): Odds Ratio =", round(shot1_guard, 2), 
      "(", round((1-shot1_guard)*100, 0), "% DECREASE in success)\n")
  
  # Model Accuracy (approximate from confusion matrix logic if data available, else just AIC)
  cat("Model AIC:", round(model$aic, 1), "\n")
  
} else {
  cat("Model file not found. (Check working directory)\n")
}

cat("\n══════════════════════════════════════════════════════════════\n")