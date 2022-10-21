

df_playerlog_raw <- read_parquet(here::here(".parquet", "PlayerGameLog.pq")) |> 
  janitor::clean_names() |> 
  mutate(
    player_id = as_factor(player_id)
    , season_id = fct_inseq(str_remove(season_id, "^2")) 
    , game_date = as.Date(game_date, format = "%b %d, %Y")
    , week = fct_inseq(paste0(season_id, str_pad(lubridate::week(game_date), 2, pad = "0")))
    , wl = as_factor(wl)
    , home = if_else(str_detect(matchup, "vs"), 1, 0)
    , team = str_extract(matchup, "\\w+")
    , opponent = str_remove(str_extract(matchup, "(@|vs.) \\w+"), "@ |vs. ")
  ) |> 
  arrange(player_id, game_date) |> 
  left_join(select(df_players, player_id = id, full_name))

# Remove anl cols
# add prev game stats
# add total season stats
# add season progress [early|mid|late]
# add starting players cols
# anl_col average