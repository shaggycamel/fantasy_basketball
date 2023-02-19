
library(here)
library(tidyverse)

source(here("data", "dataHub.R"))

df <- dh_getQuery("muck.sql")
df <- filter(df, season_id == "22022", player_id == "203954")

ggplot(df, aes(x = game_date, y = fg_pct)) +
  geom_line() +
  geom_point() +
  stat_smooth()

    
