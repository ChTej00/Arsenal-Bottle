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
  scale_fill_manual(values = c("Win" = "#EF0107", "Draw" = "#9C824A", "Loss" = "#D3D3D3")) +
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
