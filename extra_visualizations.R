# ==============================================================================
# FIGURE 4B: WIN PROBABILITY MODEL CURVES (FIXED)
# Objective: Visualize how the model values the Hammer and Score across ends
# ==============================================================================

# 1. Create a "Hypothetical Grid" of game states
plot_grid <- expand.grid(
  score_diff = -4:4,              # Score from -4 (down 4) to +4 (up 4)
  ends_remaining = 1:8,           # End 1 to End 8 (remaining)
  has_hammer = c(FALSE, TRUE)     # FIX: Must be Logical (TRUE/FALSE) to match model
)

# 2. Predict Win Probability for each state
plot_grid$win_prob <- predict(wp_model, newdata = plot_grid, type = "response")

# 3. Create the Plot
wp_curve_plot <- ggplot(plot_grid, aes(x = score_diff, y = win_prob, color = factor(has_hammer))) +
  # Draw the lines
  geom_line(linewidth = 1.2) +
  # Split into 8 small panels (one for each end)
  facet_wrap(~ ends_remaining, labeller = label_both) +
  
  # Custom Colors (Red for Hammer, Blue for Stealing)
  scale_color_manual(
    values = c("FALSE" = "#1f77b4", "TRUE" = "#d62728"),
    labels = c("Without Hammer", "With Hammer")
  ) +
  
  # Formatting
  labs(
    title = "Win Probability Model Validation",
    subtitle = "Probability of winning based on Score, Hammer, and Ends Remaining",
    x = "Score Differential (Positive = Leading)",
    y = "Win Probability",
    color = "Possession"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(wp_curve_plot)



# ==============================================================================
# FIGURE 3: TEAM CLUTCH RANKINGS (The "Scouting Report")
# Objective: Visualize which teams overperform vs. underperform under pressure
# ==============================================================================

# 1. Load Data (Team Stats)
team_stats <- readRDS("team_profiles.rds")

# 2. Create the "Lollipop Chart"
# This chart style is perfect for ranking positive vs. negative values
clutch_plot <- ggplot(team_stats, aes(x = reorder(NOC, pp_clutch_score), y = pp_clutch_score)) +
  # The "Stick"
  geom_segment(aes(xend = NOC, yend = 0), color = "gray70", size = 0.8) +
  # The "Pop" (Dot)
  geom_point(aes(color = pp_clutch_score > 0, size = abs(pp_clutch_score))) +
  
  # Color Scheme: Green for Clutch, Red for Choke
  scale_color_manual(values = c("FALSE" = "#d62728", "TRUE" = "#2ca02c"),
                     labels = c("Chokes (Worse under pressure)", "Clutch (Better under pressure)")) +
  
  # Formatting
  coord_flip() + # Flip to make it horizontal
  labs(
    title = "Power Play Clutch Rankings (PPCS)",
    subtitle = "Difference in avg. points scored: High Pressure vs. Low Pressure",
    x = NULL, # No label needed for country names
    y = "Clutch Score (Points Difference)",
    color = "Performance Category"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(), # Clean up lines
    plot.title = element_text(face = "bold", size = 14)
  ) +
  guides(size = "none") # Hide the size legend (it's intuitive)

print(clutch_plot)