

# Useful objects ----------------------------------------------------------

cols <- lst(
  id_cols = c("team_id", "player_id", "game_id")
  , tmp_cols = c("fgm", "fga", "ftm", "fta")
  , anl_cols = c("tov", "fg3m", "reb", "ast", "stl", "blk", "pts", "fg_pct", "ft_pct")
  , oth_cols = c("min", "fg3a", "fg3_pct", "oreb", "dreb", "pf", "plus_minus", "video_available")

)

cols$anl_non_perc <- stringr::str_subset(cols$anl_cols, "_pct", negate=TRUE)


