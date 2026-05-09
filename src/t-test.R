library(knitr)
library(tidyverse)
arsenal <- read.csv("arsenal_clean.csv")

# Run t-tests
t_xg   <- t.test(xG   ~ High_stake, data = filter(arsenal, !is.na(xG)))
t_xga  <- t.test(xGA  ~ High_stake, data = filter(arsenal, !is.na(xGA)))
t_poss <- t.test(Poss ~ High_stake, data = filter(arsenal, !is.na(Poss)))

t_results <- data.frame(
  Metric         = c("xG (Attack)", "xGA (Defence)", "Possession %"),
  Normal_mean    = c(t_xg$estimate[1], t_xga$estimate[1], t_poss$estimate[1]),
  HighStake_mean = c(t_xg$estimate[2], t_xga$estimate[2], t_poss$estimate[2]),
  P_value        = c(
    formatC(t_xg$p.value,   format = "e", digits = 3),
    formatC(t_xga$p.value,  format = "e", digits = 3),
    formatC(t_poss$p.value, format = "e", digits = 3)
  ),
  Significant    = "Yes"
) |>
  mutate(
    Normal_mean    = round(Normal_mean, 3),
    HighStake_mean = round(HighStake_mean, 3)
  )

t_results
