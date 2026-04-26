club_summary <- all_clubs %>%
  filter(!is.na(drop_index)) %>%
  group_by(team, stake_label) %>%
  summarise(avg_drop = mean(drop_index), .groups = "drop")

ggplot(club_summary, aes(x = team, y = avg_drop, fill = stake_label)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_text(aes(label = round(avg_drop, 2)),
            position = position_dodge(width = 0.6),
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("High Stakes" = "#EF0107", "Normal" = "#9C824A")) +
  labs(
    title    = "Performance Drop Index: Arsenal vs Liverpool vs Tottenham",
    subtitle = "Positive = underperformed season average | Negative = overperformed",
    x        = NULL,
    y        = "Average Drop Index",
    fill     = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid      = element_blank(),
    axis.line.y     = element_line(color = "grey80")
  )


heatmap_data <- all_clubs %>%
  filter(!is.na(drop_index)) %>%
  group_by(team, season, stake_label) %>%
  summarise(avg_drop = mean(drop_index), .groups = "drop") %>%
  filter(stake_label == "High Stakes")

ggplot(heatmap_data, aes(x = season, y = team, fill = avg_drop)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(avg_drop, 2)), size = 3.5, color = "black") +
  scale_fill_gradient2(
    low      = "#2166ac",
    mid      = "white",
    high     = "#EF0107",
    midpoint = 0,
    limits = c(-1,2),
    name     = "Drop Index"
  ) +
  labs(
    title    = "High Stakes Drop Index by Club and Season",
    subtitle = "Red = underperformed | Blue = overperformed",
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid  = element_blank()
  )


 line_data <- all_clubs %>%
  filter(!is.na(drop_index)) %>%
  group_by(team, season, stake_label) %>%
  summarise(avg_drop = mean(drop_index), .groups = "drop") %>%
  filter(stake_label == "High Stakes")

ggplot(line_data, aes(x = season, y = avg_drop, color = team, group = team)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = round(avg_drop, 2)),
            vjust = -0.8, size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c(
    "Arsenal"   = "#EF0107",
    "Liverpool" = "#00B2A9",
    "Tottenham" = "#132257"
  )) +
  labs(
    title    = "High Stakes Drop Index by Season",
    subtitle = "All competitions 2017-25",
    x        = NULL,
    y        = "Average Drop Index",
    color    = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid      = element_blank(),
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )


win_rate_all <- all_clubs %>%
  mutate(win = ifelse(result == "W", 1, 0)) %>%
  group_by(team, season, stake_label) %>%
  summarise(win_rate = round(mean(win) * 100, 1), .groups = "drop")

ggplot(win_rate_all, aes(x = season, y = win_rate, color = stake_label, group = stake_label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("High Stakes" = "#EF0107", "Normal" = "#9C824A")) +
  facet_wrap(~ team, nrow = 1) +
  labs(
    title    = "Win Rate by Season: High Stakes vs Normal",
    subtitle = "All competitions 2017-25",
    x        = NULL,
    y        = "Win Rate %",
    color    = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    axis.text.x     = element_text(angle = 45, hjust = 1),
    strip.text      = element_text(size = 13, face = "bold")
  )


gap_data <- all_clubs %>%
  filter(!is.na(xg), !is.na(xga), !is.na(poss)) %>%
  group_by(team, stake_label) %>%
  summarise(
    avg_xg   = mean(xg),
    avg_xga  = mean(xga),
    avg_poss = mean(poss),
    .groups  = "drop"
  ) %>%
  pivot_wider(names_from = stake_label, values_from = c(avg_xg, avg_xga, avg_poss)) %>%
  mutate(
    xg_drop   = `avg_xg_Normal`   - `avg_xg_High Stakes`,
    xga_rise  = `avg_xga_High Stakes` - `avg_xga_Normal`,
    poss_drop = `avg_poss_Normal` - `avg_poss_High Stakes`
  ) %>%
  select(team, xg_drop, xga_rise, poss_drop) %>%
  pivot_longer(cols = c(xg_drop, xga_rise, poss_drop),
               names_to = "metric", values_to = "gap") %>%
  mutate(metric = recode(metric,
                         "xg_drop"   = "xG Drop",
                         "xga_rise"  = "xGA Rise",
                         "poss_drop" = "Possession Drop"
  ))

ggplot(gap_data, aes(x = team, y = gap, fill = team)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(gap, 2)), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c(
    "Arsenal"   = "#EF0107",
    "Liverpool" = "#00B2A9",
    "Tottenham" = "#132257"
  )) +
  facet_wrap(~ metric, scales = "free_y", nrow = 1) +
  labs(
    title    = "Performance Drop-off in High Stakes Matches",
    subtitle = "How much each club declines vs their normal average",
    x        = NULL,
    y        = "Drop-off magnitude",
    fill     = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text      = element_text(size = 12, face = "bold")
  )
