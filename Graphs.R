arsenal <- read.csv("arsenal_clean.csv")
head(arsenal)
load(tidyverse)

#
# BAR GRAPH: POSS, xG, xGA: High stake vs low stake
#

layer1_summary <- arsenal |>
  filter(!is.na(xG), !is.na(xGA), !is.na(Poss)) |>
  mutate(stake_label = ifelse(High_stake == 1, "High Stake", "Normal")) |>
  group_by(stake_label) |>
  summarise(
    avg_xG = mean(xG),
    avg_xGA = mean(xGA),
    avg_Poss = mean(Poss)
  ) |>
  pivot_longer(cols = c(avg_xG, avg_xGA,avg_Poss),
               names_to = "metric",
               values_to = "value") |>
  mutate(metric = recode(metric,
                         "avg_xG" = "xG (Attack)",
                         "avg_xGA" = "xG (Defence)",
                         "avg_Poss" = "Possession %"
                         ))
layer1_summary

ggplot(layer1_summary, aes(x = stake_label, y = value, fill = stake_label)) +
  geom_col(width = 0.5) +
  scale_fill_manual(values = c("High Stake" = "red", "Normal" = "blue")) +
  facet_wrap(~ metric, scales = "free_y") +
  labs(
    title    = "Arsenal Performance: High Stakes vs Normal Matches",
    subtitle = "Average Possession, xG and xGA across all competitions (2017-25)",
    x        = NULL,
    y        = "Average Value",
    fill     = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

#
# BOX PLOT: Poss, xG, XGA: High Stake vs Normal
#

arsenal_long <- arsenal |>
  filter(!is.na(xG), !is.na(xGA), !is.na(Poss)) |>
  mutate(stake_label = ifelse(High_stake == 1, "High Stakes", "Normal")) |>
  select(stake_label, xG, xGA, Poss) |>
  pivot_longer(cols = c(xG, xGA, Poss),
               names_to = "metric",
               values_to = "value") |>
  mutate(metric = recode(metric,
                         "xG"   = "xG (Attack)",
                         "xGA"  = "xGA (Defence)",
                         "Poss" = "Possession %"
  ))

ggplot(arsenal_long, aes(x = stake_label, y = value, fill = stake_label)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2) +
  scale_fill_manual(values = c("High Stakes" = "red", "Normal" = "blue")) +
  facet_wrap(~ metric, scales = "free_y") +
  labs(
    title    = "Arsenal Performance Distribution: High Stakes vs Normal",
    subtitle = "All competitions 2017-25",
    x        = NULL,
    y        = NULL,
    fill     = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")
