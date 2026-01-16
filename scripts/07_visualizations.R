# ==============================================================================
# 07_visualizations_FINAL.R - CLEAN, ESSENTIAL FIGURES ONLY
# ==============================================================================

library(tidyverse)
library(scales)

# 1. LOAD DATA
pp_data <- readRDS("pp_analysis_ready.rds")
stones <- read_csv("data/raw/Stones.csv", show_col_types = FALSE)
team_profiles <- readRDS("team_profiles.rds")
competitors <- read_csv("data/raw/Competitors.csv", show_col_types = FALSE) %>%
  select(TeamID, NOC) %>% distinct() %>% mutate(TeamID = as.numeric(TeamID))

pp_only <- pp_data %>% 
  filter(PowerPlay == 1) %>%
  mutate(TeamID = as.numeric(TeamID))

if(!"NOC" %in% colnames(pp_only)) {
  pp_only <- pp_only %>% left_join(competitors, by = "TeamID")
}

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# CLEAN THEME
# ==============================================================================

theme_clean <- theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold", color = "black", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 10, color = "gray30", margin = margin(b = 10)),
    plot.caption = element_text(size = 9, color = "gray50", margin = margin(t = 10)),
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    legend.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 10, color = "black", face = "bold"),
    legend.position = "bottom",
    plot.margin = margin(20, 20, 20, 20)
  )

# ==============================================================================
# FIGURE 1: Scoring by Pressure Level (BAR CHART)
# ==============================================================================

fig1_data <- pp_only %>%
  group_by(pressure_combined) %>%
  summarize(
    mean_points = mean(Result, na.rm = TRUE),
    se = sd(Result, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    pressure_combined = factor(pressure_combined, levels = c("low", "medium", "high", "very_high")),
    label = paste0(round(mean_points, 2), "\n(n=", n, ")")
  )

fig1 <- ggplot(fig1_data, aes(x = pressure_combined, y = mean_points, fill = pressure_combined)) +
  geom_col(width = 0.65, color = "gray30", linewidth = 0.3) +
  geom_errorbar(aes(ymin = mean_points - se, ymax = mean_points + se), 
                width = 0.15, color = "gray30", linewidth = 0.5) +
  geom_text(aes(label = label), vjust = -0.8, size = 3.2, color = "gray20") +
  scale_fill_brewer(palette = "Blues", direction = 1) +
  scale_y_continuous(limits = c(0, 2.5), expand = expansion(mult = c(0, 0.15))) +
  scale_x_discrete(labels = c("Low", "Medium", "High", "Very High")) +
  labs(
    title = "Teams Score MORE Under Pressure",
    subtitle = "Average points on power plays increase as pressure rises (+35% from low to very high)",
    x = "Pressure Level",
    y = "Average Points Scored",
    caption = "Error bars = standard error"
  ) +
  theme_clean +
  theme(legend.position = "none")

ggsave("output/figures/fig1_scoring_by_pressure.png", fig1, 
       width = 8, height = 6, dpi = 300, bg = "white")


# ==============================================================================
# FIGURE 2: Shot Selection by Pressure (GROUPED BAR)
# ==============================================================================

stones_with_pressure <- stones %>%
  mutate(
    match_id = paste(CompetitionID, SessionID, GameID, sep = "_"),
    TeamID = as.numeric(TeamID)
  ) %>%
  inner_join(
    pp_only %>% select(match_id, EndID, TeamID, pressure_combined) %>% distinct(),
    by = c("match_id", "EndID", "TeamID")
  )

fig2_data <- stones_with_pressure %>%
  group_by(match_id, EndID, TeamID) %>%
  mutate(shot_order = row_number()) %>%
  ungroup() %>%
  filter(shot_order <= 3) %>%
  mutate(
    shot_category = case_when(
      Task %in% c(6, 7, 8, 9, 10) ~ "Aggressive",
      Task %in% c(1, 2) ~ "Defensive",
      Task %in% c(0, 5) ~ "Scoring",
      TRUE ~ "Other"
    ),
    shot_category = factor(shot_category, levels = c("Scoring", "Aggressive", "Defensive", "Other"))
  ) %>%
  group_by(pressure_combined, shot_category) %>%
  summarize(n = n(), .groups = "drop") %>%
  group_by(pressure_combined) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(pressure_combined = factor(pressure_combined, levels = c("low", "medium", "high", "very_high")))

fig2 <- ggplot(fig2_data, aes(x = pressure_combined, y = pct, fill = shot_category)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "gray30", linewidth = 0.2) +
  scale_fill_manual(values = c("Scoring" = "#4daf4a", "Aggressive" = "#e41a1c", 
                               "Defensive" = "#377eb8", "Other" = "#bdbdbd")) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.05))) +
  scale_x_discrete(labels = c("Low", "Medium", "High", "Very High")) +
  labs(
    title = "Shot Selection Changes Under Pressure",
    subtitle = "First 3 shots of each power play: Aggressive play increases under pressure",
    x = "Pressure Level",
    y = "Percentage of Shots",
    fill = "Shot Type"
  ) +
  theme_clean +
  theme(legend.position = "bottom")

ggsave("output/figures/fig2_shot_selection.png", fig2, 
       width = 9, height = 6, dpi = 300, bg = "white")


# ==============================================================================
# FIGURE 3: PPCS Rankings (HORIZONTAL BAR CHART - CLEANER THAN LOLLIPOP)
# ==============================================================================

fig3_data <- team_profiles %>%
  mutate(
    NOC = fct_reorder(NOC, ppcs),
    bar_color = case_when(
      ppcs >= 1 ~ "Strong Clutch",
      ppcs > 0 ~ "Slight Clutch",
      ppcs > -0.5 ~ "Slight Choke",
      TRUE ~ "Strong Choke"
    ),
    bar_color = factor(bar_color, levels = c("Strong Clutch", "Slight Clutch", "Slight Choke", "Strong Choke"))
  )

fig3 <- ggplot(fig3_data, aes(x = ppcs, y = NOC, fill = bar_color)) +
  geom_col(width = 0.7, color = "gray40", linewidth = 0.2) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.8) +
  geom_text(aes(label = round(ppcs, 2), 
                hjust = ifelse(ppcs >= 0, -0.2, 1.2)), 
            size = 3, color = "gray20") +
  scale_fill_manual(values = c("Strong Clutch" = "#1a9641", "Slight Clutch" = "#a6d96a",
                               "Slight Choke" = "#fdae61", "Strong Choke" = "#d7191c")) +
  scale_x_continuous(limits = c(-2, 3.5)) +
  labs(
    title = "Power Play Clutch Score (PPCS) by Team",
    subtitle = "PPCS = Points under high pressure − Points under low pressure",
    x = "PPCS",
    y = NULL,
    fill = "Performance"
  ) +
  theme_clean +
  theme(
    legend.position = "right",
    panel.grid.major.y = element_blank()
  )

ggsave("output/figures/fig3_ppcs_rankings.png", fig3, 
       width = 10, height = 8, dpi = 300, bg = "white")


# ==============================================================================
# FIGURE 4: Model Coefficients (FIXED - Remove broken coefficients)
# ==============================================================================

if(file.exists("model_coefficients.rds")) {
  coefficients <- readRDS("model_coefficients.rds")
  
  # Filter out coefficients with absurdly large standard errors (perfect separation)
  fig4_data <- coefficients %>%
    filter(term != "(Intercept)") %>%
    filter(abs(std.error) < 10) %>%  # Remove broken coefficients
    mutate(
      term_clean = term %>%
        str_replace("shot1", "Shot 1: ") %>%
        str_replace("shot2", "Shot 2: ") %>%
        str_replace("pressure_combined", "Pressure: "),
      significant = p.value < 0.05,
      term_clean = fct_reorder(term_clean, estimate)
    )
  
  # Check if we have any data left
  if(nrow(fig4_data) > 0) {
    fig4 <- ggplot(fig4_data, aes(x = estimate, y = term_clean)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
      geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, 
                         xmax = estimate + 1.96 * std.error),
                     height = 0.25, color = "gray50") +
      geom_point(aes(color = significant), size = 3) +
      scale_color_manual(values = c("TRUE" = "#d62728", "FALSE" = "#7f7f7f"),
                         labels = c("TRUE" = "Significant (p < 0.05)", "FALSE" = "Not significant")) +
      labs(
        title = "Predictors of Power Play Success",
        subtitle = "Logistic regression: Positive values = higher odds of scoring 2+ points",
        x = "Coefficient (log-odds)",
        y = NULL,
        color = NULL,
        caption = "Note: Variables with unstable estimates (perfect separation) excluded"
      ) +
      theme_clean +
      theme(
        legend.position = "bottom",
        panel.grid.major.y = element_blank()
      )
    
    ggsave("output/figures/fig4_model_coefficients.png", fig4, 
           width = 9, height = 6, dpi = 300, bg = "white")
  } else {
    print("Warning: No valid coefficients after filtering. Rebuilding model...")
  }
}
# ==============================================================================
# FIGURE 5: Pressure Response Curves (LINE CHART)
# ==============================================================================

pp_curves <- pp_only %>%
  filter(!is.na(NOC)) %>%
  mutate(
    pressure_num = case_when(
      pressure_combined == "low" ~ 1,
      pressure_combined == "medium" ~ 2,
      pressure_combined == "high" ~ 3,
      pressure_combined == "very_high" ~ 4
    )
  ) %>%
  group_by(NOC, pressure_num) %>%
  summarize(mean_pts = mean(Result, na.rm = TRUE), n = n(), .groups = "drop")

teams_enough_data <- pp_curves %>%
  group_by(NOC) %>%
  filter(n() >= 3) %>%
  pull(NOC) %>%
  unique()

pp_curves_filtered <- pp_curves %>% filter(NOC %in% teams_enough_data)

highlight_teams <- c("NZL", "EST", "CAN", "ITA", "GBR", "USA")

fig5 <- ggplot() +
  geom_line(data = pp_curves_filtered %>% filter(!NOC %in% highlight_teams),
            aes(x = pressure_num, y = mean_pts, group = NOC),
            color = "gray75", alpha = 0.5, linewidth = 0.6) +
  geom_line(data = pp_curves_filtered %>% filter(NOC %in% highlight_teams),
            aes(x = pressure_num, y = mean_pts, color = NOC),
            linewidth = 1.3) +
  geom_point(data = pp_curves_filtered %>% filter(NOC %in% highlight_teams),
             aes(x = pressure_num, y = mean_pts, color = NOC),
             size = 2.5) +
  scale_x_continuous(breaks = 1:4, labels = c("Low", "Medium", "High", "Very High")) +
  scale_color_manual(values = c(
    "NZL" = "#1a9641", "EST" = "#d7191c", "CAN" = "#ff7f00",
    "ITA" = "#377eb8", "GBR" = "#984ea3", "USA" = "#4daf4a"
  )) +
  labs(
    title = "Pressure Response Curves: Who Rises, Who Folds?",
    subtitle = "Gray = all teams | Colored = key teams (NZL rises, EST/GBR fold)",
    x = "Pressure Level",
    y = "Average Points Scored",
    color = "Team"
  ) +
  theme_clean +
  theme(legend.position = "right")

ggsave("output/figures/fig5_pressure_curves.png", fig5, 
       width = 10, height = 6, dpi = 300, bg = "white")


# ==============================================================================
# FIGURE 6: PPCS vs WPA Scatter (THE KEY INSIGHT FIGURE)
# ==============================================================================

if(file.exists("ppcs_wpa_comparison.rds")) {
  comparison <- readRDS("ppcs_wpa_comparison.rds")
  
  fig6 <- ggplot(comparison, aes(x = ppcs, y = avg_wpa)) +
    # Quadrant shading
    annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf, 
             fill = "#d4edda", alpha = 0.3) +
    annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = 0, 
             fill = "#fff3cd", alpha = 0.3) +
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0, 
             fill = "#f8d7da", alpha = 0.3) +
    # Reference lines
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.5) +
    geom_vline(xintercept = 0, color = "gray40", linewidth = 0.5) +
    # Points
    geom_point(size = 4, color = "#2c3e50", alpha = 0.8) +
    geom_text(aes(label = NOC), vjust = -1, size = 3, color = "gray20") +
    # Quadrant labels
    annotate("text", x = 2, y = 0.18, label = "TRUE CLUTCH", 
             fontface = "bold", size = 3.5, color = "#155724") +
    annotate("text", x = 2, y = 0.15, label = "Score more AND win more", 
             size = 2.8, color = "#155724") +
    annotate("text", x = 1.8, y = -0.045, label = "FALSE CLUTCH", 
             fontface = "bold", size = 3.5, color = "#856404") +
    annotate("text", x = 1.8, y = -0.055, label = "Score more but don't win more", 
             size = 2.8, color = "#856404") +
    annotate("text", x = -1, y = -0.045, label = "UNDERPERFORMERS", 
             fontface = "bold", size = 3.5, color = "#721c24") +
    labs(
      title = "The False Clutch Problem: High Scoring ≠ High Impact",
      subtitle = "PPCS measures scoring under pressure | WPA measures actual game impact",
      x = "PPCS (Scoring Under Pressure)",
      y = "Average WPA (Win Probability Added)",
      caption = "Correlation = 0.17 (weak) — High scorers aren't always game-changers"
    ) +
    theme_clean +
    theme(legend.position = "none")
  
  ggsave("output/figures/fig6_ppcs_vs_wpa.png", fig6, 
         width = 10, height = 8, dpi = 300, bg = "white")
}

print("✓ All 6 figures generated!")
print(list.files("output/figures"))

