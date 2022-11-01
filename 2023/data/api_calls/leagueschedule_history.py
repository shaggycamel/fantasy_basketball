
print('\n--------------------- leaueschedule_history')
df = pd.DataFrame()
for year in range(max_season-4, max_season+1):
    r = requests.get("http://data.nba.com/data/10s/v2015/json/mobile_teams/nba/{}/league/00_full_schedule.json".format(year))

    for month in r.json()['lscd']:
        for game in month['mscd']['g']:
            df_d = dict()
            df_d['GameDate'] = game['gdte']
            df_d['GameId'] = game['gid']
            df_d['ArenaName'] = game['an']
            df_d['ArenaState'] = game['as']
            df_d['ArenaCity'] = game['ac']
            df_d['HomeTeamId'] = game['h']['tid']
            df_d['HomeTeamName'] = game['h']['tn']
            df_d['HomeTeamCity'] = game['h']['tc']
            df_d['HomeTeamTricode'] = game['h']['ta']
            df_d['HomeTeamTime'] = game['htm']
            df_d['AwayTeamId'] = game['v']['tid']
            df_d['AwayTeamName'] = game['v']['tn']
            df_d['AwayTeamCity'] = game['v']['tc']
            df_d['AwayTeamTricode'] = game['v']['ta']
            df_d['AwayTeamTime'] = game['vtm']
        
            df = pd.concat([df, pd.DataFrame(df_d, index=[0])]).reset_index(drop=True)

df['GameDate'] = pd.to_datetime(df['GameDate']).dt.date
df['HomeTeamTime'] = pd.to_datetime(df['HomeTeamTime'])
df['AwayTeamTime'] = pd.to_datetime(df['AwayTeamTime'])
df.to_parquet('.parquet/LeagueSchedule.pq')

print('\n')