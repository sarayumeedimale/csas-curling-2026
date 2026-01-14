# ==============================================================================
# DAY 13 & 14: COMPLETE VISUALIZATIONS SUITE
# Figures 1-7: Scoring, Shot Selection, Team Rankings, Model Insights
# ==============================================================================

library(tidyverse)
library(scales)
library(broom)
library(ggrepel)

# 0. SETUP & DIRECTORIES --------------------------------------------------
# Ensure output directory exists to prevent save errors
if(!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)

# Set consistent theme
theme_csas <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "gray40", hjust = 0.5),
    axis.title = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# 1. LOAD DATA & RECOVER SCORES -------------------------------------------
print("--- Loading Data ---")
pp_data <- readRDS("pp_analysis_ready.rds")

# --- RECOVERY BLOCK (Fixes missing Result column) ---
raw_ends <- read_csv("data/raw/Ends.csv", show_col_types = FALSE) %>%
  mutate(match_id = paste(CompetitionID, SessionID, GameID, sep = "_")) %>%
  select(match_id, EndID, TeamID, Result)

if("Result" %in% colnames(pp_data)) {
  pp_data <- pp_data %>% select(-Result)
}

pp_data <- pp_data %>%
  left_join(raw_ends, by = c("match_id", "EndID", "TeamID")) %>%
  mutate(
    match_id = as.character(match_id),
    TeamID = as.character(TeamID)
  )

# Filter for Power Plays ONLY
pp_data <- pp_data %>% filter(PowerPlay == 1)

# Load Stones
stones <- read_csv("data/raw/Stones.csv", show_col_types = FALSE) %>%
  mutate(
    match_id = paste(CompetitionID, SessionID, GameID, sep = "_"),
    match_id = as.character(match_id), 
    TeamID = as.character(TeamID),
    EndID = as.numeric(EndID)
  )

# Load Team Profiles (Rankings)
team_profiles <- readRDS("team_profiles.rds")

# Load Model (for coefficients)
pp_model <- readRDS("final_predictive_model.rds")

print("✓ Data Loaded & Scores Recovered.")

# ==============================================================================
# FIGURE 1: THE CLUTCH PARADOX (Scoring by Pressure)
# ==============================================================================
print("Generating Figure 1...")

fig1_data <- pp_data %>%
  group_by(pressure_combined) %>%
  summarize(
    mean_points = mean(Result, na.rm = TRUE),
    se = sd(Result, na.rm = TRUE) / sqrt(n()),
    n = n()
  )

fig1 <- ggplot(fig1_data, aes(x = pressure_combined, y = mean_points, fill = pressure_combined)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_errorbar(aes(ymin = mean_points - se, ymax = mean_points + se), width = 0.2) +
  scale_fill_manual(values = c("low" = "#88CCEE", "medium" = "#DDCC77", "high" = "#CC6677", "very_high" = "#882255")) +
  labs(
    title = "The Clutch Paradox: Scoring Increases Under Pressure",
    subtitle = "Average points scored on Power Play ends by pressure level",
    x = "Pressure Level",
    y = "Average Points Scored"
  ) +
  theme_csas + theme(legend.position = "none")

ggsave("output/figures/fig1_clutch_paradox.png", fig1, width = 8, height = 6, bg = "white")

# ==============================================================================
# FIGURE 2: THE DEFENSIVE SHIFT (Shot Selection)
# ==============================================================================
print("Generating Figure 2...")

# Simplify shot types
shot_data <- stones %>%
  mutate(
    shot_type = case_when(
      Task %in% c(1, 2, 4, 5) ~ "Guard/Setup",
      Task %in% c(0, 3, 10)   ~ "Draw/Offense",
      Task %in% c(6, 7, 8, 9) ~ "Takeout",
      TRUE ~ "Other"
    )
  ) %>%
  inner_join(
    pp_data %>% select(match_id, EndID, TeamID, pressure_combined),
    by = c("match_id", "EndID", "TeamID")
  ) %>%
  arrange(match_id, EndID, TeamID, ShotID) %>%
  group_by(match_id, EndID, TeamID) %>%
  mutate(shot_order = row_number()) %>%
  filter(shot_order <= 3) # First 3 shots only

fig2_data <- shot_data %>%
  group_by(pressure_combined, shot_type) %>%
  summarize(count = n(), .groups = "drop") %>%
  group_by(pressure_combined) %>%
  mutate(percent = count / sum(count))

fig2 <- ggplot(fig2_data, aes(x = pressure_combined, y = percent, fill = shot_type)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("Draw/Offense" = "#44AA99", "Guard/Setup" = "#117733", "Takeout" = "#AA4499", "Other" = "gray")) +
  labs(
    title = "The Defensive Shift",
    subtitle = "Teams play more Guard/Setup shots as pressure rises",
    x = "Pressure Level",
    y = "Proportion of First 3 Shots",
    fill = "Shot Type"
  ) +
  theme_csas

ggsave("output/figures/fig2_defensive_shift.png", fig2, width = 9, height = 6, bg = "white")

# ==============================================================================
# FIGURE 3: TEAM RANKINGS (PPCS Lollipop Chart)
# ==============================================================================
print("Generating Figure 3...")

# 1. Recalculate PPCS (Safety Step)
# This ensures the column exists even if it wasn't saved in the RDS
if(!"ppcs" %in% colnames(team_profiles)) {
  team_profiles <- team_profiles %>%
    mutate(ppcs = mean_points_high - mean_points_low)
}

# 2. Prepare Plot Data
fig3_data <- team_profiles %>%
  mutate(
    color_group = case_when(
      ppcs > 0.5 ~ "Clutch (Perform Better)",
      ppcs < -0.3 ~ "Choke (Perform Worse)",
      TRUE ~ "Consistent"
    ),
    NOC = fct_reorder(NOC, ppcs)
  )

# 3. Plot
fig3 <- ggplot(fig3_data, aes(x = ppcs, y = NOC, color = color_group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_segment(aes(x = 0, xend = ppcs, y = NOC, yend = NOC), size = 1.2) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Clutch (Perform Better)" = "#117733", "Choke (Perform Worse)" = "#CC6677", "Consistent" = "gray50")) +
  labs(
    title = "Power Play Clutch Score (PPCS) by Team",
    subtitle = "Difference in Avg Points: High Pressure vs. Low Pressure",
    x = "PPCS (Positive = Better Under Pressure)",
    y = NULL,
    color = "Status"
  ) +
  theme_csas

ggsave("output/figures/fig3_ppcs_rankings.png", fig3, width = 10, height = 8, bg = "white")

# ==============================================================================
# FIGURE 4: PRESSURE VALIDATION
# ==============================================================================
print("Generating Figure 4...")

fig4_data <- pp_data %>%
  count(pressure_situation, pressure_leverage) %>%
  mutate(
    pressure_situation = factor(pressure_situation, levels = c("low", "medium", "high")),
    pressure_leverage = factor(pressure_leverage, levels = c("low", "medium", "high"))
  )

fig4 <- ggplot(fig4_data, aes(x = pressure_situation, y = pressure_leverage, fill = n)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = n), color = "white", fontface = "bold") +
  scale_fill_viridis_c(option = "magma") +
  labs(
    title = "Pressure Metric Validation",
    subtitle = "Agreement between Game Situation & Leverage Index",
    x = "Game Context (Score/End)",
    y = "Win Probability Leverage",
    fill = "Count"
  ) +
  theme_csas

ggsave("output/figures/fig4_pressure_validation.png", fig4, width = 7, height = 6, bg = "white")

# ==============================================================================
# FIGURE 5: EXECUTION QUALITY
# ==============================================================================
print("Generating Figure 5...")

fig5_data <- shot_data %>%
  group_by(pressure_combined) %>%
  summarize(
    avg_score = mean(Points, na.rm = TRUE),
    se = sd(Points, na.rm = TRUE) / sqrt(n()),
    perfect_pct = mean(Points == 4, na.rm = TRUE)
  )

fig5 <- ggplot(fig5_data, aes(x = pressure_combined, y = avg_score, fill = pressure_combined)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = avg_score - se, ymax = avg_score + se), width = 0.2) +
  geom_text(aes(label = percent(perfect_pct, accuracy = 0.1)), vjust = -1.5, size = 3.5, color = "gray30") +
  scale_fill_manual(values = c("low" = "#88CCEE", "medium" = "#DDCC77", "high" = "#CC6677", "very_high" = "#882255")) +
  coord_cartesian(ylim = c(2.5, 3.5)) + # Zoom in to see differences
  labs(
    title = "Execution Quality Under Pressure",
    subtitle = "Average shot score (0-4) and % Perfect Shots (top)",
    x = "Pressure Level",
    y = "Avg Shot Score (0-4)"
  ) +
  theme_csas + theme(legend.position = "none")

ggsave("output/figures/fig5_execution.png", fig5, width = 8, height = 6, bg = "white")

# ==============================================================================
# FIGURE 6: MODEL COEFFICIENTS (Forest Plot)
# ==============================================================================
print("Generating Figure 6...")

# Retidy the model to get Std.Error
model_data_plot <- tidy(pp_model) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term_clean = str_remove_all(term, "shot1|shot2|pressure_combined"),
    is_significant = p.value < 0.10,
    term_clean = fct_reorder(term_clean, estimate)
  )

fig6 <- ggplot(model_data_plot, aes(x = estimate, y = term_clean, color = is_significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error), height = 0.2) +
  scale_color_manual(values = c("TRUE" = "#117733", "FALSE" = "gray70")) +
  labs(
    title = "Predictors of Power Play Success",
    subtitle = "Right = Increases Odds | Left = Decreases Odds",
    x = "Log Odds (Estimate)",
    y = "Predictor",
    color = "Significant (p < 0.1)"
  ) +
  theme_csas

ggsave("output/figures/fig6_model_coefficients.png", fig6, width = 10, height = 7, bg = "white")

# ==============================================================================
# FIGURE 7: CASE STUDY (Clutch vs Choke)
# ==============================================================================
print("Generating Figure 7...")

# Manual selection based on your results (Day 11)
selected_nocs <- c("TUR", "CAN", "USA")

fig7_data <- team_profiles %>%
  filter(NOC %in% selected_nocs) %>%
  select(NOC, mean_points_low, mean_points_high) %>%
  pivot_longer(cols = c(mean_points_low, mean_points_high), names_to = "Situation", values_to = "Points") %>%
  mutate(
    Situation = ifelse(Situation == "mean_points_low", "Low Pressure", "High Pressure"),
    NOC = factor(NOC, levels = c("TUR", "CAN", "USA")) # Order for display
  )

fig7 <- ggplot(fig7_data, aes(x = NOC, y = Points, fill = Situation)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_hline(yintercept = 2.0, linetype = "dashed", color = "black", alpha = 0.5) +
  annotate("text", x = 0.6, y = 2.1, label = "Target (2.0)", size = 3) +
  scale_fill_manual(values = c("Low Pressure" = "#88CCEE", "High Pressure" = "#CC6677")) +
  labs(
    title = "Case Study: The 'Clutch' Difference",
    subtitle = "Turkey rises under pressure; USA falls.",
    x = "Team",
    y = "Avg Points per Power Play"
  ) +
  theme_csas

ggsave("output/figures/fig7_case_study.png", fig7, width = 8, height = 6, bg = "white")

print("✓ ALL VISUALIZATIONS COMPLETE. Check 'output/figures/' folder.")