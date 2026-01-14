# ==============================================================================
# 09_export_model_summary.R
# Purpose: Exports statistical model results to a text file for reporting/AI
# ==============================================================================

library(tidyverse)
library(broom)

# 1. SETUP & LOAD
# Ensure output directory exists
if(!dir.exists("output")) dir.create("output")

# Load the model
if(file.exists("final_predictive_model.rds")) {
  pp_model <- readRDS("final_predictive_model.rds")
  print("✓ Model loaded: final_predictive_model.rds")
} else {
  stop("❌ ERROR: 'final_predictive_model.rds' not found. Please check your file name.")
}

# 2. GENERATE TEXT REPORT
output_file <- "output/model_results_for_claude.txt"
sink(output_file) # Redirect output to file

cat("================================================================\n")
cat(" FINAL PREDICTIVE MODEL SUMMARY\n")
cat(" Target: Power Play Success (2+ Points)\n")
cat("================================================================\n\n")

cat("--- A. MODEL METRICS ---\n")
print(glance(pp_model))
cat("\n")

cat("--- B. COEFFICIENTS (LOG ODDS) ---\n")
# Standard summary showing significance stars
print(summary(pp_model))
cat("\n")

cat("--- C. ODDS RATIOS (INTERPRETABLE EFFECT SIZES) ---\n")
cat("Values > 1.0 indicate INCREASED probability of success.\n")
cat("Values < 1.0 indicate DECREASED probability of success.\n\n")

odds_ratios <- tidy(pp_model, exponentiate = TRUE, conf.int = TRUE) %>%
  select(term, estimate, p.value, conf.low, conf.high) %>%
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    ),
    estimate = round(estimate, 3),
    p.value = round(p.value, 4)
  ) %>%
  arrange(desc(estimate))

print(as.data.frame(odds_ratios), row.names = FALSE)

sink() # Stop redirecting

print(paste("✓ Success! Model summary saved to:", output_file))