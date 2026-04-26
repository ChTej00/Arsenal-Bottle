library(tidyverse)
arsenal <- read.csv("arsenal_clean.csv")
liverpool <- read.csv("liverpool.csv")
spurs <- read.csv("spurs.csv")

head(arsenal)
head(liverpool)
head(spurs)

calc_pdi <- function(df) {
  season_avgs <- df %>%
    filter(!is.na(xg), !is.na(xga), !is.na(poss)) %>%
    group_by(season) %>%
    summarise(
      season_avg_xg   = mean(xg),
      season_avg_xga  = mean(xga),
      season_avg_poss = mean(poss),
      .groups = "drop"
    )
  
  df %>%
    left_join(season_avgs, by = "season") %>%
    mutate(
      drop_index  = (season_avg_xg - xg) +
        (xga - season_avg_xga) +
        (season_avg_poss - poss) / 10,
      stake_label = ifelse(high_stake == 1, "High Stakes", "Normal")
    )
}

colnames(arsenal) <- tolower(colnames(arsenal))
head(arsenal)

arsenal   <- calc_pdi(arsenal)
liverpool <- calc_pdi(liverpool)
spurs     <- calc_pdi(spurs)

all_clubs <- bind_rows(arsenal, liverpool, spurs)

arsenal <- arsenal |>
  mutate(date = as.Date(date,format = "%Y-%m-%d"))

liverpool <- liverpool %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

spurs <- spurs %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

all_clubs %>% count(team) %>% as.data.frame()

# High stake counts per club
all_clubs %>%
  group_by(team) %>%
  summarise(
    total     = n(),
    high_stake = sum(high_stake),
    normal     = sum(high_stake == 0)
  ) %>%
  as.data.frame()

# Average drop index per club
all_clubs %>%
  filter(!is.na(drop_index)) %>%
  group_by(team, stake_label) %>%
  summarise(avg_drop = round(mean(drop_index), 3), .groups = "drop") %>%
  as.data.frame()
