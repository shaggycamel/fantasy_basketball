from nba_api.stats.endpoints import playergamelog

# list to iterate over
it_lst = nba_players_anl

print('\n--------------------- playergamelog')
df = pd.DataFrame() 
for i in range(0, len(it_lst)):
    for y in range(max_season-4, max_season+1):
        player_gamelog = playergamelog.PlayerGameLog(
            player_id=str(it_lst['id'][i])
            , season=y
            , season_type_all_star='Regular Season'
            , timeout=timeout
        )
        p = player_gamelog.data_sets[0].get_data_frame()
        df = pd.concat([df, p], ignore_index=True)
    if i % 50 == 0 and y == max_season: print('player:', i, '/', len(it_lst))
print('player:', i, '/', len(it_lst))

df.to_parquet('.parquet/PlayerGameLog.pq')
print('\n')