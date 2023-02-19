SELECT 
    log.season_id
    , log.player_id
    , player.player_slug
    , log.game_id
    , log.game_date
    , log.matchup
    , log.wl
    , log.min
    , log.fgm
    , log.fga
    , log.fg_pct
    , log.fg3m
    , log.fg3a
    , log.fg3_pct
    , log.ftm
    , log.fta
    , log.ft_pct
    , log.oreb
    , log.dreb
    , log.reb
    , log.ast
    , log.stl
    , log.blk
    , log.tov
    , log.pf
    , log.pts
FROM nba.player_game_log AS log
LEFT JOIN nba.player_info AS player ON log.player_id = player.person_id::TEXT