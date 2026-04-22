arsenal <- read.csv("arsenal_clean.csv")
head(arsenal)
library(tidyverse)
library(patchwork)

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



