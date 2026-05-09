# Updated bar chart
title_gap <- data.frame(
  season         = c("2022-23", "2023-24", "2024-25"),
  points_dropped = c(12, 9, 11),
  title_gap      = c(5, 2, 10)
)

title_gap_long <- title_gap %>%
  pivot_longer(cols = c(points_dropped, title_gap),
               names_to = "type", values_to = "value") %>%
  mutate(type = recode(type,
                       "points_dropped" = "Points Dropped in High Stakes",
                       "title_gap"      = "Points Behind Title Winner"
  ))

ggplot(title_gap_long, aes(x = season, y = value, fill = type)) +
  geom_col(position = "dodge", width = 0.5) +
  geom_text(aes(label = value),
            position = position_dodge(width = 0.5),
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c(
    "Points Dropped in High Stakes" = "#EF0107",
    "Points Behind Title Winner"    = "#9C824A"
  )) +
  labs(
    title    = "The Cost of Bottling — Title Race Seasons",
    subtitle = "Points dropped in high stakes PL matches vs final title gap",
    x        = NULL,
    y        = "Points",
    fill     = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid      = element_blank()
  )

# Updated table
data.frame(
  Season           = c("2022-23", "2023-24", "2024-25"),
  High_Stake_Games = c(10, 11, 12),
  Max_Points       = c(30, 33, 36),
  Points_Earned    = c(18, 24, 25),
  Points_Dropped   = c(12, 9, 11),
  Title_Gap        = c(5, 2, 10)
) %>%
  kable(col.names = c(
    "Season", "High Stakes Games",
    "Max Points", "Points Earned",
    "Points Dropped", "Lost Title By"
  ))
