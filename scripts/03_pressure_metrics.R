# scripts/03_pressure_metrics.R
# Day 5A: Pressure Definition 1 (Game Situation)

library(tidyverse)

# 1. Load the data --------------------------------------------------------
# Based on your GitHub screenshot, the latest file is "wp_predictions_v2.rds"
# This file should contain the game data + the win probabilities Sarayu calculated.
pp_data <- readRDS("wp_predictions_v2.rds")

# 2. Define Pressure Definition 1: Game Situation -------------------------
# Logic from Plan:
# High: 2 or fewer ends left AND score within 2 points
# Medium: 3 or fewer ends left OR score within 3 points
# Low: Everything else

pp_data <- pp_data %>%
  mutate(
    # Ensure ends_remaining is numeric (just in case)
    ends_remaining = as.numeric(ends_remaining),
    
    pressure_situation = case_when(
      ends_remaining <= 2 & abs(score_diff) <= 2 ~ "high",
      ends_remaining <= 3 | abs(score_diff) <= 3 ~ "medium",
      TRUE ~ "low"
    )
  )

# 3. Quick Validation -----------------------------------------------------
# Check the distribution to make sure it worked
print("Distribution of Game Situation Pressure:")
table(pp_data$pressure_situation)

# =========================================================================
# DAY 6A: PRESSURE DEFINITION 2 (WIN PROBABILITY LEVERAGE)
# =========================================================================

# FIXED: Removed the accidental nested "pp_data <-" line
pp_data <- pp_data %>%
  mutate(
    # 1. Calculate Leverage Score (Uncertainty)
    # 1.0 = Pure 50/50 Coin Flip. 0.0 = 100% Certain Outcome.
    leverage_score = 1 - (2 * abs(0.5 - wp_pred)),
    
    # 2. Classify Leverage
    pressure_leverage = case_when(
      leverage_score >= quantile(leverage_score, 0.75, na.rm = TRUE) ~ "high",
      leverage_score >= quantile(leverage_score, 0.50, na.rm = TRUE) ~ "medium",
      TRUE ~ "low"
    )
  )

# Validation
print("Crosstab: Situation vs Leverage")
table(pp_data$pressure_situation, pp_data$pressure_leverage)

# =========================================================================
# DAY 7A: COMBINED PRESSURE METRIC & FINALIZATION
# =========================================================================

pp_data <- pp_data %>%
  mutate(
    pressure_combined = case_when(
      # LOGIC FIX:
      # If it is "Game Situation High" (Late & Close) AND not a complete blowout (Low Leverage)...
      # ...we call it VERY HIGH. This captures the "Clutch" moments (300+ shots).
      pressure_situation == "high" & pressure_leverage != "low" ~ "very_high",
      
      # If it's just Late & Close (but desperate/low leverage), it's still High Pressure.
      pressure_situation == "high" ~ "high",
      
      # If it's a mid-game swinger (High Leverage), it's High Pressure.
      pressure_leverage == "high" ~ "high",
      
      # Everything else
      pressure_situation == "medium" | pressure_leverage == "medium" ~ "medium",
      TRUE ~ "low"
    )
  )

# Set Factor Levels
pp_data$pressure_combined <- factor(pp_data$pressure_combined, 
                                    levels = c("low", "medium", "high", "very_high"))

# Final Validation (You should now see ~300-400 Very High)
print("Distribution of Final Combined Pressure:")
table(pp_data$pressure_combined)

# SAVE
saveRDS(pp_data, "pp_analysis_ready.rds")

