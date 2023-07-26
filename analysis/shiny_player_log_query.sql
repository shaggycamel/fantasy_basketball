SELECT 
    slug_season, 
    season_type,
    RIGHT(slug_season, 2) || '-' || season_type AS year_season_type,
    player_id,
    info.display_first_last AS player_name,
    game_date, 
    game_id, 
    wl,  
    min,
    fgm, 
    fga, 
    fg_pct, 
    fg3_m, 
    fg3_a, 
    fg3_pct, 
    ft_pct, 
    ftm, 
    fta, 
    oreb, 
    dreb, 
    reb, 
    ast, 
    stl, 
    blk, 
    tov, 
    pf, 
    pts
FROM nba.player_game_log AS log
LEFT JOIN nba.player_info AS info ON log.player_id = info.person_id
WHERE LEFT(slug_season, 4)::INT >= 2021
  AND season_type != 'All Star'