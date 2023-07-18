SELECT 
    slug_season, 
    id_player_nba,
    FA.PLAYER_ID AS fty_player_id,
    name_player,  
    slug_teams_bref,
    slug_position, 
    age_player, 
    count_games, 
    count_games_started, 
    minutes_totals,
    fgm_totals, 
    fga_totals, 
    fg3m_totals, 
    fg3a_totals, 
    fg2m_totals, 
    fg2a_totals, 
    ftm_totals, 
    fta_totals, 
    orb_totals, 
    drb_totals, 
    trb_totals, 
    ast_totals, 
    stl_totals, 
    blk_totals, 
    tov_totals, 
    pf_totals, 
    pts_totals,
    pct_fg, 
    pct_fg3, 
    pct_fg2, 
    pct_efg, 
    pct_ft
FROM nba.player_season_stats AS stats
LEFT JOIN fty.FREE_AGENTS AS FA ON stats.NAME_PLAYER = FA.PLAYER_NAME
    AND FA.PLAYER_STATUS = 'ACTIVE'
WHERE SLUG_SEASON = '2022-23' 
ORDER BY ID_PLAYER_NBA, SLUG_SEASON  
