SELECT 
    slug_season, 
    id_player_nba,
    id_matchup.fty_id,
    fa.player_status AS free_agent_status,
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
LEFT JOIN util.fty_nba_id_matchup AS id_matchup ON stats.id_player_nba = id_matchup.nba_id 
LEFT JOIN fty.free_agents AS fa ON id_matchup.fty_id = fa.player_id::INT
WHERE slug_season = '2022-23' 
ORDER BY id_player_nba, slug_season  
