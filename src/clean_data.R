arsenal <- read.csv("arsenal_match_results.csv")


head(arsenal)
library(tidyverse)

arsenal <- arsenal |>
  mutate(date = as.Date(Date, format = "%d-%m-%Y")) |>
  mutate(season = case_when(
    date >= as.Date("2017-08-01") & date <= as.Date("2018-05-31") ~ "2017-18",
    date >= as.Date("2018-08-01") & date <= as.Date("2019-05-31") ~ "2018-19",
    date >= as.Date("2019-08-01") & date <= as.Date("2020-07-31") ~ "2019-20",
    date >= as.Date("2020-08-01") & date <= as.Date("2021-05-31") ~ "2020-21",
    date >= as.Date("2021-08-01") & date <= as.Date("2022-05-31") ~ "2021-22",
    date >= as.Date("2022-08-01") & date <= as.Date("2023-05-31") ~ "2022-23",
    date >= as.Date("2023-08-01") & date <= as.Date("2024-05-31") ~ "2023-24",
    date >= as.Date("2024-08-01") & date <= as.Date("2025-05-31") ~ "2024-25",
    TRUE ~ "unknown"
  ))

arsenal <- arsenal |>
  mutate(Date = as.Date(Date, format = "%d-%m-%Y"))

arsenal <- arsenal |> select(-date)

arsenal <- arsenal |>
  filter(!is.na(Result)) |>
  mutate(Points = case_when(
    Comp == "Premier League" & Result == "W" ~ 3,
    Comp == "Premier League" & Result == "D" ~ 1,
    Comp == "Premier League" & Result == "D" ~ 0
  ))

arseal <- arsenal |>
  mutate(
    xG_diff = xG - xGA,
    Home = case_when(
      Comp == "Premier League" & Venue != "@" ~ 1,
      Comp == "Premier League" & Venue == "@" ~ 0
    ),
    Gameweek = 
         )
assign("arsenal_backup",arsenal)

arsenal <- arsenal |> mutate(xG_diff = xG- xGA)

arsenal <- arsenal |>
  mutate(matchweek_num = if_else(
    str_detect(Round, "^Matchweek"),
    as.numeric(str_extract(Round, "\\d+")),
    NA_real_
  ))
head(arsenal)

unique(arsenal$Comp)

#Defining High-Stake Matches

arsenal <- arsenal |>
  mutate(High_stake = case_when(
    #Prem: Last 6 gameweeks
    Comp == "Premier League" & matchweek_num >= 33 ~ 1,
    
    #Domestic cups: Quater Final onwards
    Comp %in% c("FA Cup", "EFL Cup") &
      Round %in% c("Quarter-Finals", "Semi-Finals", "Final") ~ 1,
    
    #London Derby-s + Man City cause why not
    Comp %in% c("FA Cup", "EFL Cup", "Premier League") &
      Opp %in% c("Chelsea", "Tottenham Hotspur", "Manchester City") ~ 1,
    
    #European Cups: R16 Ondwards
    Comp %in% c("Champions League", "Europa League") &
      Round %in% c("Round of 16","Quarter-Finals", "Semi-Finals", "Final") ~ 1,
    
    TRUE ~ 0
    
  ))

(filter(arsenal,High_stake == 1))

head(arsenal)

#SANITY CHECKS

# 1. How many high stakes vs normal?
table(arsenal$High_stake)

# 2. High stakes breakdown by competition
arsenal %>%
  filter(High_stake == 1) %>%
  count(Comp, Round) %>%
  arrange(Comp, Round) %>%
  as.data.frame()

arsenal |> filter(Comp == "Europa League")

# 3. Season counts — do all 8 seasons look right?
arsenal %>%
  count(season)

# 4. Any missing xG or xGA values?
arsenal |>
  summarise(
    missing_xg  = sum(is.na(xG)),
    missing_xga = sum(is.na(xGA)),
    missing_poss = sum(is.na(Poss))
  )

arsenal |> filter(is.na(xG)) 

# 5. Quick check — are Big Six PL matches being caught?
arsenal %>%
  filter(High_stake == 1) %>%
  count(Opp) %>%
  arrange(desc(n))

write_csv(arsenal, "arsenal_clean.csv")


#manually added some missing xG values
#filled in half of them, other half doesnt exist

arsenal <- read.csv("arsenal_clean.csv")

head(arsenal)

table(arsenal$High_stake)

arsenal %>%
  filter(High_stake == 1) %>%
  count(Comp) %>%
  as.data.frame()

arsenal %>% count(season) %>% as.data.frame()
