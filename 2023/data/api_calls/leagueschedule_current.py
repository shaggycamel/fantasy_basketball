r = requests.get("https://cdn.nba.com/static/json/staticData/scheduleLeagueV2.json")

print('\n--------------------- leaueschedule_current')
df = pd.DataFrame()
for dt in r.json()['leagueSchedule']['gameDates']:
    df_d = dict()
    df_d['GameDate'] = dt['gameDate']
    
    for gm in dt['games']:
        df_d['GameId'] = gm['gameId']
        df_d['ArenaName'] = gm['arenaName']
        df_d['ArenaState'] = gm['arenaState']
        df_d['ArenaName'] = gm['arenaName']
        df_d['ArenaCity'] = gm['arenaCity']
        df_d['HomeTeamId'] = gm['homeTeam']['teamId']
        df_d['HomeTeamName'] = gm['homeTeam']['teamName']
        df_d['HomeTeamCity'] = gm['homeTeam']['teamCity']
        df_d['HomeTeamTricode'] = gm['homeTeam']['teamTricode']
        df_d['HomeTeamSlug'] = gm['homeTeam']['teamSlug']
        df_d['HomeTeamTime'] = gm['homeTeamTime']
        df_d['AwayTeamId'] = gm['awayTeam']['teamId']
        df_d['AwayTeamName'] = gm['awayTeam']['teamName']
        df_d['AwayTeamCity'] = gm['awayTeam']['teamCity']
        df_d['AwayTeamTricode'] = gm['awayTeam']['teamTricode']
        df_d['AwayTeamSlug'] = gm['awayTeam']['teamSlug']
        df_d['AwayTeamTime'] = gm['awayTeamTime']
        
        df = pd.concat([df, pd.DataFrame(df_d, index=[0])]).reset_index(drop=True)

df['GameDate'] = pd.to_datetime(df['GameDate']).dt.date
df['HomeTeamTime'] = pd.to_datetime(df['HomeTeamTime'])
df['AwayTeamTime'] = pd.to_datetime(df['AwayTeamTime'])
# df.to_parquet('.parquet/LeagueSchedule.pq')

print('\n')