

# Useful objects ----------------------------------------------------------

id_cols <- c("player_id", "min")
tmp_cols <- c("fgm", "fga", "ftm", "fta")
oth_cols <- c("fg3a", "fg3_pct", "oreb", "dreb", "pf", "plus_minus", "video_available")
anl_cols <- c("tov", "fg3m", "reb", "ast", "stl", "blk", "pts", "fg_pct", "ft_pct")
anl_non_perc <- stringr::str_subset(anl_cols, "_pct", negate=TRUE)


