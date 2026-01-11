# ==============================================================================
# REPORT STATISTICS GENERATOR
# Run this to get all numbers for your report
# ==============================================================================

library(tidyverse)

pp_data <- readRDS("pp_analysis_ready.rds")
team_profiles <- readRDS("team_profiles.rds")

# Filter to PP only
pp_only <- pp_data %>% filter(PowerPlay == 1)

cat("══════════════════════════════════════════════════════════════\n")
cat("📊 ALL STATS FOR REPORT - COPY THESE\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# DATA OVERVIEW
cat("--- DATA OVERVIEW ---\n")
cat("Total power play ends:", nrow(pp_only), "\n")
cat("Teams analyzed:", nrow(team_profiles), "\n\n")

# SCORING BY PRESSURE
cat("--- SCORING BY PRESSURE ---\n")
scoring <- pp_only %>%
  group_by(pressure_combined) %>%
  summarize(
    n = n(),
    mean_pts = round(mean(Result, na.rm = TRUE), 2),
    success_rate = round(mean(Result >= 2, na.rm = TRUE) * 100, 1)
  )
print(scoring)

low <- scoring %>% filter(pressure_combined == "low") %>% pull(mean_pts)
high <- scoring %>% filter(pressure_combined == "high") %>% pull(mean_pts)
very_high <- scoring %>% filter(pressure_combined == "very_high") %>% pull(mean_pts)
decline <- round((low - very_high) / low * 100, 1)

cat("\nLow pressure avg:", low, "pts\n")
cat("High pressure avg:", high, "pts\n")
cat("Very high pressure avg:", very_high, "pts\n")
cat("Decline low→very high:", decline, "%\n\n")

# PPCS RANKINGS
cat("--- PPCS RANKINGS ---\n")
cat("\nTOP 3 CLUTCH:\n")
top3 <- team_profiles %>% slice_head(n = 3)
for(i in 1:3) cat(i, ". ", top3$NOC[i], " (PPCS = +", round(top3$ppcs[i], 2), ")\n", sep="")

cat("\nBOTTOM 3 CHOKERS:\n")
bot3 <- team_profiles %>% slice_tail(n = 3) %>% arrange(ppcs)
for(i in 1:3) cat(i, ". ", bot3$NOC[i], " (PPCS = ", round(bot3$ppcs[i], 2), ")\n", sep="")

cat("\nTeams better under pressure:", sum(team_profiles$ppcs > 0), "\n")
cat("Teams worse under pressure:", sum(team_profiles$ppcs < 0), "\n")

# MODEL STATS (if exists)
if(file.exists("model_evaluation.rds")) {
  model_eval <- readRDS("model_evaluation.rds")
  cat("\n--- MODEL STATS ---\n")
  cat("Accuracy:", round(model_eval$accuracy * 100, 1), "%\n")
  cat("AUC:", round(model_eval$auc, 3), "\n")
}

cat("\n══════════════════════════════════════════════════════════════\n")
