
SELECT 
	st.SEASON_ID,
	nm.DISPLAY_FIRST_LAST AS player_name,
	st.gp,
	st.gs,
	st.min,
	st.fgm,
	st.fga,
	st.FG3_M,
	st.fg3_a,
	st.FTM,
	st.FTA,
	st.pts,
	st.PF,
	st.oreb,
	st.dreb,
	st.reb,
	st.ast,
	st.stl,
	st.blk,
	st.tov	
	
FROM nba.player_season_stats AS st
LEFT JOIN nba.player_info AS nm ON st.PLAYER_ID = nm.PERSON_ID 
WHERE LOWER(nm.DISPLAY_FIRST_LAST) LIKE 'n%jokic'

UNION ALL

SELECT 
	st.SEASON_ID,
	'season_total' AS player_name,
	SUM(st.gp),
	SUM(st.gs),
	SUM(st.min),
	SUM(st.fgm),
	SUM(st.fga),
	SUM(st.FG3_M),
	SUM(st.fg3_a),
	SUM(st.FTM),
	SUM(st.FTA),
	SUM(st.pts),
	SUM(st.PF),
	SUM(st.oreb),
	SUM(st.dreb),
	SUM(st.reb),
	SUM(st.ast),
	SUM(st.stl),
	SUM(st.blk),
	SUM(st.tov)	
	
FROM nba.player_season_stats AS st
GROUP BY st.SEASON_ID 
