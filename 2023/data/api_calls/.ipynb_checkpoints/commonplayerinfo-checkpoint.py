from nba_api.stats.endpoints import commonplayerinfo

print('\n--------------------- commonplayerinfo')
df = pd.DataFrame()
for i in range(0, len(nba_players)):
    player_info = commonplayerinfo.CommonPlayerInfo(player_id=str(nba_players[i]['id']), timeout=timeout)
    p = player_info.data_sets[0].get_data_frame()
    df = pd.concat([df, p], ignore_index=True)
    if i % 50 == 0: print('player:', i, '/', len(nba_players))
print('player:', i, '/', len(nba_players))
    
df.to_parquet('.parquet/PlayerInfo.pq')
print('\n')