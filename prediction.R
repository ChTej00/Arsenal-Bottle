class(arsenal$date)
head(arsenal$date)
summary(arsenal$points)

arsenal_pl_model <- arsenal %>%
  filter(comp == "Premier League", !is.na(drop_index)) %>%
  arrange(date) %>%
  mutate(
    underperform  = ifelse(drop_index > 0, 1, 0),
    recent_form   = zoo::rollmean(points, k = 5, fill = NA, align = "right")
  ) %>%
  filter(!is.na(recent_form), !is.na(matchweek_num))

nrow(arsenal_pl_model)
# Verify
class(arsenal$date)
summary(arsenal$points)


head(arsenal_model)

arsenal_pl_model <- arsenal_pl_model |>
  mutate(home = ifelse(venue == "@",0,1))

model_pl <- glm(underperform ~ high_stake + home + recent_form + matchweek_num,
                data = arsenal_pl_model,
                family = binomial)

summary(model_pl)

arsenal_2526 <- read.csv("arsenal25-26.csv") %>%
  rename_with(tolower) %>%
  mutate(
    date          = as.Date(date, format = "%d-%m-%Y"),
    matchweek_num = as.numeric(str_extract(round, "\\d+")),
    points        = case_when(
      result == "W" ~ 3,
      result == "D" ~ 1,
      result == "L" ~ 0,
      TRUE          ~ NA_real_
    ),
    high_stake    = case_when(
      matchweek_num >= 33 ~ 1,
      opp %in% c("Tottenham Hotspur", "Chelsea", "Manchester City") ~ 1,
      TRUE ~ 0
    )
  ) %>%
  arrange(date)
arsenal_2526


arsenal_2526_played <- arsenal_2526 %>%
  filter(!is.na(result)) %>%
  mutate(recent_form = zoo::rollmean(points, k = 5, fill = NA, align = "right"))
arsenal_2526_played

tail(arsenal_2526_played %>% select(date, opp, result, points, recent_form, matchweek_num, high_stake), 10)

last_form <- tail(arsenal_2526_played$recent_form, 6)[1]  
last_form

fixtures <- arsenal_2526 %>%
  filter(result == "" | is.na(result)) %>%
  mutate(
    recent_form   = last_form,
    high_stake    = as.numeric(high_stake),
    home          = as.numeric(home),
    matchweek_num = as.numeric(matchweek_num)
  )

fixtures$bottle_prob <- predict(model_pl, 
                                newdata = fixtures[, c("high_stake", "home", "recent_form", "matchweek_num")], 
                                type = "response")
fixtures %>%
  mutate(
    venue       = ifelse(home == 1, "Home", "Away"),
    bottle_prob = round(bottle_prob * 100, 1),
    risk        = case_when(
      bottle_prob >= 60 ~ "High",
      bottle_prob >= 40 ~ "Medium",
      TRUE              ~ "Low"
    )
  ) %>%
  select(date, opp, venue, matchweek_num, high_stake, bottle_prob, risk) %>%
  kable(col.names = c("Date", "Opponent", "Venue", "Matchweek", "High Stake", "Bottle %", "Risk"))
