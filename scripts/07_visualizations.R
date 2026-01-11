# ==============================================================================
# DAY 13S: CORE VISUALIZATIONS (Sarayu)
# Figures 1-2: Scoring by pressure, Shot selection by pressure
# ==============================================================================

library(tidyverse)
library(scales)

# Set consistent theme
theme_csas <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    axis.title = element_text(size = 11),
    legend.position = "bottom"
  )

# 1. LOAD DATA
pp_data <- readRDS("pp_analysis_ready.rds")
stones <- read_csv("data/raw/Stones.csv", show_col_types = FALSE)

# ==============================================================================
# FIGURE 1: Power Play Points by Pressure Level
# ==============================================================================

fig1_data <- pp_data %>%
  filter(PowerPlay == 1) %>%
  group_by(pressure_combined) %>%
  summarize(
    mean_points = mean(Result, na.rm = TRUE),
    se = sd(Result, na.rm = TRUE) / sqrt(n()),
    n = n()
  )

fig1 <- ggplot(fig1_data, aes(x = pressure_combined, y = mean_points, fill = pressure_combined)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = mean_points - se, ymax = mean_points + se), width = 0.2) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("low" = "#2ecc71", "medium" = "#f39c12", 
                               "high" = "#e74c3c", "very_high" = "#8e44ad")) +
  labs(
    title = "Power Play Scoring Drops Under Pressure",
    subtitle = "Average points scored on power play ends by pressure level",
    x = "Pressure Level",
    y = "Average Points Scored",
    caption = "Error bars show standard error"
  ) +
  theme_csas +
  theme(legend.position = "none") +
  ylim(0, max(fig1_data$mean_points) * 1.3)

print(fig1)
ggsave("output/figures/fig1_scoring_by_pressure.png", fig1, width = 8, height = 6, dpi = 300)

# ==============================================================================
# FIGURE 2: Shot Selection by Pressure (First 3 Shots)
# ==============================================================================

# Prep shot data
shot_data <- stones %>%
  mutate(
    match_id = paste(CompetitionID, SessionID, GameID, sep = "_"),
    shot_type = case_when(
      Task %in% c(6, 7, 8, 9, 10) ~ "Aggressive",
      Task %in% c(1, 2) ~ "Defensive",
      Task %in% c(0, 5) ~ "Scoring",
      TRUE ~ "Other"
    )
  ) %>%
  inner_join(
    pp_data %>% filter(PowerPlay == 1) %>% select(match_id, EndID, TeamID, pressure_combined),
    by = c("match_id", "EndID", "TeamID")
  ) %>%
  arrange(match_id, EndID, TeamID, ShotID) %>%
  group_by(match_id, EndID, TeamID) %>%
  mutate(shot_order = row_number()) %>%
  filter(shot_order <= 3) %>%
  ungroup()

fig2_data <- shot_data %>%
  count(pressure_combined, shot_type) %>%
  group_by(pressure_combined) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

fig2 <- ggplot(fig2_data, aes(x = pressure_combined, y = pct, fill = shot_type)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("Aggressive" = "#e74c3c", "Defensive" = "#3498db", 
                               "Scoring" = "#2ecc71", "Other" = "#95a5a6")) +
  labs(
    title = "Shot Selection Shifts Under Pressure",
    subtitle = "Distribution of shot types in first 3 stones of power play",
    x = "Pressure Level",
    y = "Percentage of Shots",
    fill = "Shot Type"
  ) +
  theme_csas

print(fig2)
ggsave("output/figures/fig2_shot_selection.png", fig2, width = 10, height = 6, dpi = 300)

# ==============================================================================
# FIGURE 5: Execution Quality by Pressure
# ==============================================================================

fig5_data <- shot_data %>%
  group_by(pressure_combined) %>%
  summarize(
    avg_execution = mean(Points, na.rm = TRUE),
    pct_perfect = mean(Points == 4, na.rm = TRUE),
    se = sd(Points, na.rm = TRUE) / sqrt(n()),
    n = n()
  )

fig5 <- ggplot(fig5_data, aes(x = pressure_combined, y = avg_execution, fill = pressure_combined)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = avg_execution - se, ymax = avg_execution + se), width = 0.2) +
  scale_fill_manual(values = c("low" = "#2ecc71", "medium" = "#f39c12", 
                               "high" = "#e74c3c", "very_high" = "#8e44ad")) +
  labs(
    title = "Shot Execution Quality Under Pressure",
    subtitle = "Average execution score (0-4 scale) by pressure level",
    x = "Pressure Level",
    y = "Average Execution Score",
    caption = "4 = perfect shot, 0 = miss"
  ) +
  theme_csas +
  theme(legend.position = "none")

print(fig5)
ggsave("output/figures/fig5_execution_quality.png", fig5, width = 8, height = 6, dpi = 300)

print("✓ Day 13S Complete: Figures 1, 2, 5 saved")


# ==============================================================================
# DAY 13A: TEAM & MODEL VISUALIZATIONS (Armaan)
# Figures 3-4, 6: PPCS rankings, Pressure validation, Model coefficients
# ==============================================================================

library(ggrepel)

# 1. LOAD DATA
team_profiles <- readRDS("team_profiles.rds")
coefficients <- readRDS("model_coefficients.rds")

# ==============================================================================
# FIGURE 3: Team PPCS Rankings (Lollipop Chart)
# ==============================================================================

fig3_data <- team_profiles %>%
  mutate(
    color_group = case_when(
      ppcs > 0.5 ~ "Clutch",
      ppcs < -0.3 ~ "Chokes",
      TRUE ~ "Neutral"
    ),
    NOC = fct_reorder(NOC, ppcs)
  )

fig3 <- ggplot(fig3_data, aes(x = ppcs, y = NOC, color = color_group)) +
  geom_segment(aes(x = 0, xend = ppcs, y = NOC, yend = NOC), size = 1) +
  geom_point(size = 4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Clutch" = "#2ecc71", "Chokes" = "#e74c3c", "Neutral" = "#95a5a6")) +
  labs(
    title = "Power Play Clutch Score (PPCS) by Team",
    subtitle = "Positive = better under pressure, Negative = worse under pressure",
    x = "PPCS (points difference: high pressure - low pressure)",
    y = "Team",
    color = "Category"
  ) +
  theme_csas +
  theme(legend.position = "right")

print(fig3)
ggsave("output/figures/fig3_ppcs_rankings.png", fig3, width = 10, height = 8, dpi = 300)

# ==============================================================================
# FIGURE 4: Pressure Definition Validation (Correlation)
# ==============================================================================

pp_data <- readRDS("pp_analysis_ready.rds")

fig4_data <- pp_data %>%
  filter(PowerPlay == 1) %>%
  count(pressure_situation, pressure_leverage) %>%
  mutate(
    pressure_situation = factor(pressure_situation, levels = c("low", "medium", "high")),
    pressure_leverage = factor(pressure_leverage, levels = c("low", "medium", "high"))
  )

fig4 <- ggplot(fig4_data, aes(x = pressure_situation, y = pressure_leverage, fill = n)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = n), color = "white", size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#3498db", high = "#e74c3c") +
  labs(
    title = "Pressure Definitions Agree",
    subtitle = "Crosstab of Game Situation vs Win Probability Leverage",
    x = "Definition 1: Game Situation",
    y = "Definition 2: WP Leverage",
    fill = "Count"
  ) +
  theme_csas +
  theme(legend.position = "right")

print(fig4)
ggsave("output/figures/fig4_pressure_validation.png", fig4, width = 8, height = 6, dpi = 300)

# ==============================================================================
# FIGURE 6: Model Coefficients (Forest Plot)
# ==============================================================================

fig6_data <- coefficients %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term_clean = str_remove(term, "shot1|shot2|pressure_combined"),
    significant = ifelse(p.value < 0.05, "Significant", "Not Significant"),
    term_clean = fct_reorder(term_clean, estimate)
  )

fig6 <- ggplot(fig6_data, aes(x = estimate, y = term_clean, color = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, 
                     xmax = estimate + 1.96 * std.error), height = 0.2) +
  scale_color_manual(values = c("Significant" = "#e74c3c", "Not Significant" = "#95a5a6")) +
  labs(
    title = "Predictors of Power Play Success",
    subtitle = "Logistic regression coefficients (positive = increases success odds)",
    x = "Coefficient Estimate",
    y = "Predictor",
    color = "p < 0.05"
  ) +
  theme_csas

print(fig6)
ggsave("output/figures/fig6_model_coefficients.png", fig6, width = 10, height = 7, dpi = 300)

# ==============================================================================
# FIGURE 7 (BONUS): Case Study Highlight
# ==============================================================================

selected_teams <- readRDS("selected_case_study_teams.rds")
case_moments <- readRDS("case_study_moments.rds")

# Bar chart comparing clutch vs choke team
fig7_data <- team_profiles %>%
  filter(NOC %in% selected_teams) %>%
  select(NOC, mean_points_low, mean_points_high) %>%
  pivot_longer(cols = c(mean_points_low, mean_points_high), 
               names_to = "pressure", values_to = "points") %>%
  mutate(
    pressure = ifelse(pressure == "mean_points_low", "Low Pressure", "High Pressure"),
    NOC = factor(NOC, levels = selected_teams)
  )

fig7 <- ggplot(fig7_data, aes(x = NOC, y = points, fill = pressure)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Low Pressure" = "#3498db", "High Pressure" = "#e74c3c")) +
  labs(
    title = "Case Study: Clutch vs Choke Teams",
    subtitle = paste("Comparing", selected_teams["clutch"], "(Clutch) vs", 
                     selected_teams["choke"], "(Chokes) vs",
                     selected_teams["contender"], "(Contender)"),
    x = "Team",
    y = "Average Points on Power Play",
    fill = "Situation"
  ) +
  theme_csas

print(fig7)
ggsave("output/figures/fig7_case_study.png", fig7, width = 9, height = 6, dpi = 300)


# ==============================================================================
# DAY 14S: FIGURES 5-6 (Sarayu)
# ==============================================================================

# FIGURE 5: Execution Quality by Pressure (Bar chart with error bars)

execution_data <- stones %>%
  mutate(match_id = paste(CompetitionID, SessionID, GameID, sep = "_")) %>%
  inner_join(
    pp_data %>% filter(PowerPlay == 1) %>% select(match_id, EndID, TeamID, pressure_combined),
    by = c("match_id", "EndID", "TeamID")
  ) %>%
  group_by(pressure_combined) %>%
  summarize(
    avg_execution = mean(Points, na.rm = TRUE),
    se = sd(Points, na.rm = TRUE) / sqrt(n()),
    pct_perfect = mean(Points == 4, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

fig5 <- ggplot(execution_data, aes(x = pressure_combined, y = avg_execution, fill = pressure_combined)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = avg_execution - se, ymax = avg_execution + se), width = 0.2) +
  geom_text(aes(label = paste0(round(pct_perfect * 100, 1), "% perfect")), 
            vjust = -0.5, size = 3, color = "gray30") +
  scale_fill_manual(values = c("low" = "#2ecc71", "medium" = "#f39c12", 
                               "high" = "#e74c3c", "very_high" = "#8e44ad")) +
  labs(
    title = "Shot Execution Quality Drops Under Pressure",
    subtitle = "Average execution score (0-4 scale) by pressure level",
    x = "Pressure Level",
    y = "Average Execution Score",
    caption = "Percentage shows proportion of perfect shots (score = 4)"
  ) +
  theme_csas +
  theme(legend.position = "none") +
  ylim(0, 4)

print(fig5)
ggsave("output/figures/fig5_execution_quality.png", fig5, width = 8, height = 6, dpi = 300)


# FIGURE 6: Model Coefficient Plot (Forest plot)

coefficients <- readRDS("model_coefficients.rds")

fig6_data <- coefficients %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term_clean = str_remove_all(term, "shot1|shot2|pressure_combined"),
    significant = ifelse(p.value < 0.05, "p < 0.05", "Not significant"),
    direction = ifelse(estimate > 0, "Increases success", "Decreases success"),
    term_clean = fct_reorder(term_clean, estimate)
  )

fig6 <- ggplot(fig6_data, aes(x = estimate, y = term_clean, color = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, 
                     xmax = estimate + 1.96 * std.error), height = 0.2) +
  scale_color_manual(values = c("p < 0.05" = "#e74c3c", "Not significant" = "#95a5a6")) +
  labs(
    title = "What Predicts Power Play Success?",
    subtitle = "Logistic regression coefficients (positive = higher odds of scoring 2+ points)",
    x = "Coefficient (log-odds)",
    y = "Predictor",
    color = "Significance"
  ) +
  theme_csas

print(fig6)
ggsave("output/figures/fig6_model_coefficients.png", fig6, width = 10, height = 7, dpi = 300)

print("✓ Day 14S Complete: Figures 5, 6 saved")



# ==============================================================================
# DAY 14A: FIGURE 7 + FINAL REVIEW (Armaan)
# ==============================================================================

# FIGURE 7: Case Study Comparison

selected_teams <- readRDS("selected_case_study_teams.rds")
team_profiles <- readRDS("team_profiles.rds")

fig7_data <- team_profiles %>%
  filter(NOC %in% selected_teams) %>%
  select(NOC, mean_points_low, mean_points_high, ppcs) %>%
  pivot_longer(
    cols = c(mean_points_low, mean_points_high),
    names_to = "pressure",
    values_to = "points"
  ) %>%
  mutate(
    pressure = ifelse(pressure == "mean_points_low", "Low Pressure", "High Pressure"),
    pressure = factor(pressure, levels = c("Low Pressure", "High Pressure")),
    NOC = factor(NOC, levels = c(selected_teams["clutch"], selected_teams["contender"], selected_teams["choke"]))
  )

fig7 <- ggplot(fig7_data, aes(x = NOC, y = points, fill = pressure)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "gray50", size = 0.5) +
  annotate("text", x = 0.5, y = 2.1, label = "Success threshold (2 pts)", 
           hjust = 0, size = 3, color = "gray40") +
  scale_fill_manual(values = c("Low Pressure" = "#3498db", "High Pressure" = "#e74c3c")) +
  labs(
    title = "Case Study: Clutch vs Choke Performance",
    subtitle = paste0(selected_teams["clutch"], " (Clutch) vs ", 
                      selected_teams["contender"], " (Contender) vs ",
                      selected_teams["choke"], " (Chokes)"),
    x = "Team",
    y = "Average Points on Power Play",
    fill = "Situation"
  ) +
  theme_csas

print(fig7)
ggsave("output/figures/fig7_case_study.png", fig7, width = 9, height = 6, dpi = 300)

