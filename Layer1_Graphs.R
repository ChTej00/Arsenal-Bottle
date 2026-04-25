arsenal <- read.csv("arsenal_clean.csv")
head(arsenal)
library(tidyverse)
library(patchwork)

#
# BOX PLOT: xG, xGA, Poss High stakes vs Normal
#

arsenal_long <- arsenal %>%
  filter(!is.na(xG), !is.na(xGA), !is.na(Poss)) %>%
  mutate(stake_label = ifelse(High_stake == 1, "High Stakes", "Normal")) %>%
  select(stake_label, xG, xGA, Poss) %>%
  pivot_longer(cols = c(xG, xGA, Poss),
               names_to = "metric",
               values_to = "value") %>%
  mutate(metric = recode(metric,
                         "xG"   = "xG (Attack)",
                         "xGA"  = "xGA (Defence)",
                         "Poss" = "Possession %"
  ))

# xG and xGA only — same scale
p1 <- arsenal_long %>%
  filter(metric %in% c("xG (Attack)", "xGA (Defence)")) %>%
  ggplot(aes(x = stake_label, y = value, fill = stake_label)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2) +
  scale_fill_manual(values = c("High Stakes" = "#EF0107", "Normal" = "#9C824A")) +
  facet_wrap(~ metric) +
  labs(x = NULL, y = "xG Value", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

# Possession only
p2 <- arsenal_long %>%
  filter(metric == "Possession %") %>%
  ggplot(aes(x = stake_label, y = value, fill = stake_label)) +
  geom_boxplot(alpha = 0.7, width = 0.7, outlier.shape = 21, outlier.size = 2) +
  scale_fill_manual(values = c("High Stakes" = "#EF0107", "Normal" = "#9C824A")) +
  labs(x = NULL, y = "Possession %", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# Combine — xG panels on left, possession on right
p1 + p2 +
  plot_layout(widths = c(2,1))+
  plot_annotation(
    title    = "Arsenal Performance Distribution: High Stakes vs Normal",
    subtitle = "All competitions 2017-25"
  )

#
# DEFINING NEW METRIC: DROP INDEX
# SCATTER PLOT, DROP INDEX VS DATE
#

season_avgs <- arsenal |>
  filter(!is.na(xG),!is.na(xGA),!is.na(Poss)) |>
  group_by(season) |>
  summarise(
    season_avg_xg = mean(xG),
    season_avg_xga = mean(xGA),
    season_avg_poss = mean(Poss)
  )

season_avgs

arsenal_pdi <- arsenal|>
  filter(!is.na(xG),!is.na(xGA),!is.na(Poss)) |>
  left_join(season_avgs, by = "season") |>
  mutate(
    drop_index = (season_avg_xg - xG) +
      (xGA - season_avg_xga) +
      (season_avg_poss - Poss) /10 ,
    stake_label = ifelse(High_stake == 1, "High Stakes", "Normal")
  )
arsenal_pdi

arsenal_pdi |>
  group_by(stake_label) |>
  summarise(avg_drop_index = round(mean(drop_index),3)) |>
  as.data.frame()

arteta_date <- as.Date("2019-12-20")

season_starts <- as.Date(c(
  "2017-08-01", "2018-08-01", "2019-08-01", "2020-08-01",
  "2021-08-01", "2022-08-01", "2023-08-01", "2024-08-01"
))

season_labels <- c("17-18", "18-19", "19-20", "20-21",
                   "21-22", "22-23", "23-24", "24-25")

ggplot(arsenal_pdi, aes(x = Date, y = drop_index, color = stake_label)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = season_starts, linetype = "dotted", 
             color = "grey60", linewidth = 0.5) +
  geom_vline(xintercept = arteta_date,
             linetype = "dashed", color = "black", linewidth = 0.7) +
  annotate("text", x = season_starts, 
           y = max(arsenal_pdi$drop_index) * 1.0,
           label = season_labels, size = 3, color = "grey40", hjust = -0.1) +
  annotate("text", x = arteta_date, 
           y = max(arsenal_pdi$drop_index) * 0.85,
           label = "Arteta\nAppointed", hjust = -0.1, size = 3.5, color = "black") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("High Stakes" = "#EF0107", "Normal" = "#9C824A")) +
  labs(
    title    = "Performance Drop Index Over Time",
    subtitle = "Positive = underperformed season average | Negative = overperformed",
    x        = NULL,
    y        = "Drop Index",
    color    = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "grey80")
  )

class(arsenal_pdi$Date)
arsenal_pdi <- arsenal_pdi %>%
  mutate(Date = as.Date(Date, format = "%d-%m-%Y"))


#
#  PIE CHART: WIN RATE, WIN VS LOSS VS DRAW
#

win_rate <- arsenal %>%
  mutate(stake_label = ifelse(High_stake == 1, "High Stakes", "Normal"),
         outcome = case_when(
           Result == "W" ~ "Win",
           Result == "D" ~ "Draw",
           Result == "L" ~ "Loss"
         )) %>%
  count(stake_label, outcome) %>%
  group_by(stake_label) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

ggplot(win_rate, aes(x = 2, y = pct, fill = outcome)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  facet_wrap(~ stake_label) +
  scale_fill_manual(values = c("Win" = "#EF0107", "Draw" = "#D3D3D3", "Loss" = "#9C824A")) +
  geom_text(aes(label = paste0(pct, "%")), 
            position = position_stack(vjust = 0.5), size = 4) +
  labs(
    title = "Arsenal Win Rate: High Stakes vs Normal Matches",
    subtitle = "All competitions 2017-25",
    fill = NULL,
    x = NULL,
    y = NULL
  ) +
  theme_void(base_size = 13) +
  theme(legend.position = "top",
        strip.text = element_text(size = 13, face = "bold"))


win_rate_season <- arsenal %>%
  mutate(
    stake_label = ifelse(High_stake == 1, "High Stakes", "Normal"),
    win = ifelse(Result == "W", 1, 0)
  ) %>%
  group_by(season, stake_label) %>%
  summarise(win_rate = round(mean(win) * 100, 1), .groups = "drop")

ggplot(win_rate_season, aes(x = season, y = win_rate, fill = stake_label)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = paste0(win_rate, "%")), 
            position = position_dodge(width = 0.6),
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("High Stakes" = "#EF0107", "Normal" = "#9C824A")) +
  labs(
    title    = "Win Rate by Season: High Stakes vs Normal",
    subtitle = "All competitions 2017-25",
    x        = NULL,
    y        = "Win Rate %",
    fill     = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
