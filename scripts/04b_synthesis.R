# ==============================================================================
# DAY 10S: SYNTHESIS & KEY FINDINGS (Sarayu)
# Objective: Compile results from Strategy, Execution, and Sequence analyses
# ==============================================================================

library(tidyverse)

# 1. LOAD SAVED RESULTS ---------------------------------------------------
# Load the .rds files created in Days 8 and 9
strategy_data  <- readRDS("shot_strategy_summary.rds")
execution_data <- readRDS("execution_quality_summary.rds")
sequence_data  <- readRDS("shot_sequences_summary.rds")

# 2. EXTRACT KEY METRICS --------------------------------------------------

# A. Strategy: How much do they use "Guards"?
# We look specifically at the "Guard" percentage
guard_usage <- strategy_data %>%
  filter(shot_type_name == "Guard") %>%
  select(pressure_combined, guard_pct = pct)

# B. Execution: What is the average score (0-4)?
exec_score <- execution_data %>%
  select(pressure_combined, avg_score, pct_perfect)

# C. Sequence: What is the #1 Strategy?
# We take the most common opening sequence for each pressure level
top_sequences <- sequence_data %>%
  group_by(pressure_combined) %>%
  slice(1) %>% 
  select(pressure_combined, top_sequence = sequence)

# D. Sequence: What is the #2 Strategy? (The "Alternative")
second_sequences <- sequence_data %>%
  group_by(pressure_combined) %>%
  slice(2) %>% 
  select(pressure_combined, second_sequence = sequence)

# 3. BUILD THE MASTER SUMMARY TABLE ---------------------------------------
final_summary <- guard_usage %>%
  left_join(exec_score, by = "pressure_combined") %>%
  left_join(top_sequences, by = "pressure_combined") %>%
  left_join(second_sequences, by = "pressure_combined") %>%
  mutate(
    guard_pct = scales::percent(guard_pct, accuracy = 0.1),
    avg_score = round(avg_score, 2),
    pct_perfect = scales::percent(pct_perfect, accuracy = 0.1)
  )

print("--- FINAL PROJECT SYNTHESIS: The Impact of Pressure ---")
print(final_summary)

# 4. IDENTIFY HEADLINE FINDING --------------------------------------------
# Compare Low vs. Very High to find the biggest change
low_perf  <- final_summary %>% filter(pressure_combined == "low")
very_high_perf <- final_summary %>% filter(pressure_combined == "very_high")

print("--- THE HEADLINE FINDING ---")
cat(paste0(
  "When pressure shifts from LOW to VERY HIGH:\n",
  "1. CLUTCH FACTOR: Players DO NOT choke. Perfect shot % rises from ", low_perf$pct_perfect, " to ", very_high_perf$pct_perfect, ".\n",
  "2. DEFENSIVE PIVOT: Teams panic-switch to Guards. Usage jumps from ", low_perf$guard_pct, " to ", very_high_perf$guard_pct, ".\n",
  "3. STRATEGY SHIFT: The backup plan changes from '", low_perf$second_sequence, "' (Aggressive) to '", very_high_perf$second_sequence, "' (Defensive)."
))