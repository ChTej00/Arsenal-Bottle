arsenal_scatter <- arsenal %>%
  filter(!is.na(xG)) %>%
  mutate(stake_label = ifelse(High_stake == 1, "High Stakes", "Normal"))

ggplot() +
  geom_jitter(
    data = filter(arsenal_scatter, stake_label == "Normal"),
    aes(x = xG_diff, y = GD, color = stake_label),
    alpha = 0.5, size = 2.5, width = 0.05, height = 0.15
  ) +
  geom_jitter(
    data = filter(arsenal_scatter, stake_label == "High Stakes"),
    aes(x = xG_diff, y = GD, color = stake_label),
    alpha = 0.7, size = 2.5, width = 0.05, height = 0.15
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("High Stakes" = "#EF0107", "Normal" = "#9C824A")) +
  labs(
    title    = "Difference in xG and xGA vs Actual Goal Difference",
    subtitle = "Points below the line = goal difference less than expected",
    x        = "Expected Difference in Goals (xG-xGA)",
    y        = "Actual Goal Difference",
    color    = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

arsenal_scatter %>%
  mutate(below_line = GD < xG_diff) %>%
  group_by(stake_label, below_line) %>%
  summarise(n = n()) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  as.data.frame()

nrow(arsenal_scatter)
arsenal_scatter %>%
  count(stake_label)