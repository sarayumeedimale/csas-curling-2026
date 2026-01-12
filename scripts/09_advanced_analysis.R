# ==============================================================================
# ADVANCED ANALYSIS: WPA Impact + Pressure Response Curves
# ==============================================================================

library(tidyverse)

# 1. LOAD DATA
pp_data <- readRDS("pp_analysis_ready.rds")
team_profiles <- readRDS("team_profiles.rds")
competitors <- read_csv("data/raw/Competitors.csv", show_col_types = FALSE) %>%
  select(TeamID, NOC) %>% 
  distinct() %>%
  mutate(TeamID = as.numeric(TeamID))

# Check if NOC already exists, if not add it
pp_only <- pp_data %>% 
  filter(PowerPlay == 1) %>%
  mutate(TeamID = as.numeric(TeamID))

if(!"NOC" %in% colnames(pp_only)) {
  pp_only <- pp_only %>% left_join(competitors, by = "TeamID")
}

# Verify NOC exists now
print("Columns in pp_only:")
print(colnames(pp_only))
print(paste("NOC column exists:", "NOC" %in% colnames(pp_only)))

# ==============================================================================
# PART A: WIN PROBABILITY ADDED (WPA) ANALYSIS
# ==============================================================================

team_wpa <- pp_only %>%
  filter(!is.na(NOC)) %>%
  group_by(NOC) %>%
  summarize(
    n_pp = n(),
    total_wpa = sum(wpa, na.rm = TRUE),
    avg_wpa = mean(wpa, na.rm = TRUE),
    avg_wpa_high_pressure = mean(wpa[pressure_combined %in% c("high", "very_high")], na.rm = TRUE),
    avg_wpa_low_pressure = mean(wpa[pressure_combined == "low"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_pp >= 5) %>%
  mutate(ppis = avg_wpa_high_pressure - avg_wpa_low_pressure) %>%
  arrange(desc(avg_wpa))

# Compare PPCS vs WPA
comparison <- team_profiles %>%
  select(NOC, ppcs) %>%
  left_join(team_wpa %>% select(NOC, avg_wpa, ppis), by = "NOC") %>%
  filter(!is.na(avg_wpa)) %>%
  mutate(
    ppcs_rank = rank(-ppcs),
    wpa_rank = rank(-avg_wpa)
  )

cor_value <- cor(comparison$ppcs, comparison$avg_wpa, use = "complete.obs")

saveRDS(team_wpa, "team_wpa_analysis.rds")
saveRDS(comparison, "ppcs_wpa_comparison.rds")

# ==============================================================================
# PART B: PRESSURE RESPONSE CURVES
# ==============================================================================

pp_only <- pp_only %>%
  mutate(
    pressure_continuous = case_when(
      pressure_combined == "low" ~ 0.125,
      pressure_combined == "medium" ~ 0.375,
      pressure_combined == "high" ~ 0.625,
      pressure_combined == "very_high" ~ 0.875
    )
  )

pressure_curves <- pp_only %>%
  filter(!is.na(NOC)) %>%
  group_by(NOC, pressure_combined, pressure_continuous) %>%
  summarize(
    n = n(),
    mean_pts = mean(Result, na.rm = TRUE),
    mean_wpa = mean(wpa, na.rm = TRUE),
    .groups = "drop"
  )

teams_with_range <- pressure_curves %>%
  group_by(NOC) %>%
  filter(n() >= 3) %>%
  ungroup()

highlight_teams <- c("NZL", "EST", "CAN", "ITA", "USA", "SWE")

# ==============================================================================
# VISUALIZATIONS
# ==============================================================================

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# FIGURE 8: Pressure Response Curves
fig8 <- ggplot() +
  geom_line(data = teams_with_range %>% filter(!NOC %in% highlight_teams),
            aes(x = pressure_continuous, y = mean_pts, group = NOC),
            color = "gray70", alpha = 0.5, linewidth = 0.8) +
  geom_line(data = teams_with_range %>% filter(NOC %in% highlight_teams),
            aes(x = pressure_continuous, y = mean_pts, color = NOC),
            linewidth = 1.5) +
  geom_point(data = teams_with_range %>% filter(NOC %in% highlight_teams),
             aes(x = pressure_continuous, y = mean_pts, color = NOC),
             size = 3) +
  scale_x_continuous(
    breaks = c(0.125, 0.375, 0.625, 0.875),
    labels = c("Low", "Medium", "High", "Very High")
  ) +
  scale_color_manual(values = c(
    "NZL" = "#00B140", "EST" = "#0072CE", "CAN" = "#FF0000",
    "ITA" = "#009246", "USA" = "#3C3B6E", "SWE" = "#006AA7"
  )) +
  labs(
    title = "Pressure Response Curves: How Teams React as Stakes Rise",
    subtitle = "Gray lines = all teams | Colored = key teams to watch",
    x = "Pressure Level",
    y = "Average Points Scored",
    color = "Team"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )

ggsave("output/figures/fig8_pressure_curves.png", fig8, width = 10, height = 6, dpi = 300)

# FIGURE 9: PPCS vs WPA Scatter
fig9 <- ggplot(comparison, aes(x = ppcs, y = avg_wpa)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(color = ppcs > 0 & avg_wpa > 0), size = 4, alpha = 0.8) +
  geom_text(aes(label = NOC), vjust = -0.8, size = 3) +
  scale_color_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
                     labels = c("Underperforms", "True Clutch"),
                     name = "Category") +
  labs(
    title = "Clutch Scoring vs. Game Impact",
    subtitle = "Top-right quadrant = True clutch performers",
    x = "PPCS (Scoring Under Pressure)",
    y = "Average WPA (Win Probability Added)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )

ggsave("output/figures/fig9_ppcs_vs_wpa.png", fig9, width = 9, height = 7, dpi = 300)

# FIGURE 10: WPA Rankings
fig10_data <- team_wpa %>%
  mutate(
    NOC = fct_reorder(NOC, avg_wpa),
    impact_type = ifelse(avg_wpa > 0, "Positive Impact", "Negative Impact")
  )

fig10 <- ggplot(fig10_data, aes(x = avg_wpa, y = NOC, fill = impact_type)) +
  geom_col() +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
  scale_fill_manual(values = c("Positive Impact" = "#2ecc71", "Negative Impact" = "#e74c3c")) +
  labs(
    title = "Power Play Impact Score: Who Actually Changes Games?",
    subtitle = "Average Win Probability Added per power play",
    x = "Average WPA",
    y = "Team",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )

ggsave("output/figures/fig10_wpa_rankings.png", fig10, width = 9, height = 7, dpi = 300)

# ==============================================================================
# PRINT RESULTS
# ==============================================================================

print("--- TOP 5 WPA ---")
print(head(team_wpa %>% select(NOC, n_pp, avg_wpa), 5))

print("--- BOTTOM 5 WPA ---")
print(tail(team_wpa %>% select(NOC, n_pp, avg_wpa), 5))

print(paste("PPCS vs WPA correlation:", round(cor_value, 3)))

print("--- TRUE CLUTCH (High PPCS + High WPA) ---")
print(comparison %>% filter(ppcs > 0, avg_wpa > 0) %>% select(NOC, ppcs, avg_wpa) %>% arrange(desc(ppcs)))

print("--- DONE ---")
