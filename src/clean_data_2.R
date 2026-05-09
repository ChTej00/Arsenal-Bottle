clean_club_data <- function(df, club_name) {
  df %>%
    rename_with(tolower) %>%
    filter(!str_detect(tolower(comp), "community shield")) %>%
    mutate(
      date     = as.Date(date, format = "%d-%m-%Y"),
      team     = club_name,
      season   = case_when(
        date >= as.Date("2017-08-01") & date <= as.Date("2018-05-31") ~ "2017-18",
        date >= as.Date("2018-08-01") & date <= as.Date("2019-05-31") ~ "2018-19",
        date >= as.Date("2019-08-01") & date <= as.Date("2020-08-31") ~ "2019-20",
        date >= as.Date("2020-08-01") & date <= as.Date("2021-05-31") ~ "2020-21",
        date >= as.Date("2021-08-01") & date <= as.Date("2022-05-31") ~ "2021-22",
        date >= as.Date("2022-08-01") & date <= as.Date("2023-05-31") ~ "2022-23",
        date >= as.Date("2023-08-01") & date <= as.Date("2024-05-31") ~ "2023-24",
        date >= as.Date("2024-08-01") & date <= as.Date("2025-05-31") ~ "2024-25",
        TRUE ~ NA_character_
      ),
      points = case_when(
        result == "W" ~ 3,
        result == "D" ~ 1,
        result == "L" ~ 0
      ),
      xg_diff    = xg - xga,
      home       = ifelse(is.na(venue) | venue == "", 1, 0),
      matchweek_num = as.numeric(str_extract(round, "\\d+")),
      high_stake = case_when(
        comp == "Premier League" & matchweek_num >= 33 ~ 1,
        comp == "Premier League" & opp %in% c(
          "Arsenal", "Tottenham Hotspur", "Chelsea",
          "Manchester City", "Liverpool", "Manchester Utd"
        ) ~ 1,
        comp %in% c("FA Cup", "EFL Cup") & str_detect(
          round, "Quarter|Semi|Final"
        ) ~ 1,
        comp %in% c("Champions League", "Europa League") & str_detect(
          round, "Round of 16|Quarter|Semi|Final"
        ) ~ 1,
        TRUE ~ 0
      )
    ) %>%
    filter(!is.na(season))
}

liverpool <- read.csv("liverpool.csv")
spurs <- read.csv("spurs.csv")


liverpool_clean <- clean_club_data(liverpool, "Liverpool")
head(liverpool_clean)
liverpool_clean <- liverpool_clean |>
  mutate( high_stake = case_when(
    opp %in% c("Everton", "Manchester Utd", "Arsenal") ~ 1,
    TRUE ~high_stake
  ))


spurs_clean <- clean_club_data(spurs, "Tottenham")
head(spurs_clean)

spurs_clean <- spurs_clean |>
  mutate(high_stake = case_when(
    opp %in% c("Arsenal", "Chelsea", "West Ham") ~ 1,
    TRUE ~ high_stake
  ))


liverpool_clean %>% count(season) %>% as.data.frame()
spurs_clean %>% count(season) %>% as.data.frame()

# High stake breakdown by competition
liverpool_clean %>%
  filter(high_stake == 1) %>%
  count(comp) %>%
  as.data.frame()

spurs_clean %>%
  filter(high_stake == 1) %>%
  count(comp) %>%
  as.data.frame()


liverpool_clean

# Liverpool high stake opponents
liverpool_clean %>%
  filter(high_stake == 1, comp == "Premier League") %>%
  count(opp) %>%
  arrange(desc(n)) %>%
  as.data.frame()

# Spurs high stake opponents
spurs_clean %>%
  filter(high_stake == 1, comp == "Premier League") %>%
  count(opp) %>%
  arrange(desc(n)) %>%
  as.data.frame()

write.csv(liverpool_clean, "liverpool.csv")
?write.csv
write.csv(spurs_clean, "spurs.csv")
