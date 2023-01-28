from nba_api.stats.endpoints import boxscoretraditionalv2

nba_games = (pd.read_parquet('.parquet/TeamGameLog.pq')['Game_ID']
             .drop_duplicates().reset_index(drop=True))

df = pd.DataFrame()
for i in range(0, len(nba_games)):
    boxscore_trad = boxscoretraditionalv2.BoxScoreTraditionalV2(
        game_id = nba_games[i]
        , timeout=timeout
    )
    p = boxscore_trad.data_sets[0].get_data_frame()
    df = pd.concat([df, p], ignore_index=True)
    if i % 25 == 0: print('game:', i, '/', len(nba_games))
print('game:', i, '/', len(nba_games))
    
df.to_parquet('.parquet/boxscoretraditionalv2.pq')