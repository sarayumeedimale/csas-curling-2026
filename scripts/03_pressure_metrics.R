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
# Date: Jan 2
# =========================================================================

# 1. LOAD INTERMEDIATE DATA (If starting fresh today) ---------------------
# If you just opened RStudio, uncomment the line below to load yesterday's work:
# pp_data <- readRDS("pp_pressure_step1.rds")

# 2. CALCULATE LEVERAGE ---------------------------------------------------
# Leverage = How much the Win Probability (WP) changes during this end.
# Formula: |WP_after - WP_before|
# Logic: If a Power Play swings the game from 50% to 90% win prob, that was HIGH pressure.
# Note: We need 'wp_before' and 'wp_after'.
# assumption: 'wp_predictions_v2.rds' (loaded yesterday) already has these columns.
# If not, we calculate leverage using the 'wp' column generally.

# We will assume 'wp' is the probability at the start of the end.
# However, true leverage requires knowing the outcome.
# SIMPLIFICATION (as per risk mitigation plan):
# We will use the pre-game WP model to estimate "Potential Swing".
# But for now, let's look at the columns Sarayu gave us.
# (If columns are missing, we'll create a placeholder leverage based on score/end tightness).

# Let's inspect column names first to be safe (Run this line in console if unsure):
# colnames(pp_data)

# ACTUAL CALCULATION (Based on Plan Logic):
pp_data <- pp_data %>%
  mutate(
    # We define leverage roughly as: The importance of the current state.
    # Higher leverage = Late game + Close score (similar to Def 1 but continuous).
    # Using a proxy since we might not have 'wp_after' for every single shot yet.
    # Proxy: 1 / (abs(score_diff) + 1) * (EndID / 8)
    # real_leverage_metric (if WP exists):
    leverage_proxy = (1 / (abs(score_diff) + 0.5)) * (as.numeric(EndID) / 2),
    
    # Classify based on quantiles (Top 25% = High, Next 25% = Medium)
    pressure_leverage = case_when(
      leverage_proxy >= quantile(leverage_proxy, 0.75, na.rm = TRUE) ~ "high",
      leverage_proxy >= quantile(leverage_proxy, 0.50, na.rm = TRUE) ~ "medium",
      TRUE ~ "low"
    )
  )

# 3. VALIDATION -----------------------------------------------------------
print("Distribution of Leverage Pressure:")
table(pp_data$pressure_leverage)

# Check agreement between the two definitions (Are they similar?)
print("Crosstab: Situation (Rows) vs Leverage (Cols)")
table(pp_data$pressure_situation, pp_data$pressure_leverage)

# =========================================================================
# DAY 7A: COMBINED PRESSURE METRIC & FINALIZATION
# Date: Jan 3
# =========================================================================

# 1. LOAD PREVIOUS STEP (Only run this if you restarted RStudio today) ----
# pp_data <- readRDS("pp_pressure_step2.rds")

# 2. CREATE COMBINED PRESSURE INDICATOR -----------------------------------
# Logic from Plan Page 10:
# - If BOTH are High -> "very_high" (The most critical moments)
# - If EITHER is High -> "high"
# - If EITHER is Medium -> "medium"
# - Else -> "low"

pp_data <- pp_data %>%
  mutate(
    pressure_combined = case_when(
      pressure_situation == "high" & pressure_leverage == "high" ~ "very_high",
      pressure_situation == "high" | pressure_leverage == "high" ~ "high",
      pressure_situation == "medium" | pressure_leverage == "medium" ~ "medium",
      TRUE ~ "low"
    )
  )

# Set the factor order so graphs look right later (Low -> Very High)
pp_data$pressure_combined <- factor(pp_data$pressure_combined, 
                                    levels = c("low", "medium", "high", "very_high"))

# 3. FINAL VALIDATION -----------------------------------------------------
# Check the counts. We expect fewer "very_high" than "high".
print("Distribution of Final Combined Pressure:")
table(pp_data$pressure_combined)

# 4. SAVE FINAL ANALYSIS DATASET ------------------------------------------
# This is the 'Golden Dataset' Sarayu needs for the Core Analysis.
saveRDS(pp_data, "pp_analysis_ready.rds")

