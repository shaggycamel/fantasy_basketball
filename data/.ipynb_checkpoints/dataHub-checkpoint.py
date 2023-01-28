
import configparser
import espn_api.basketball as bb
from os import getcwd
from pathlib import Path
from pandas import DataFrame, concat
from sqlalchemy import create_engine
from importlib.machinery import SourceFileLoader

# timeout = 3600 + 600 # 1hour & 10mins


class dataHub:
    
    def __init__(self):
        self
        
    def db_connect(self):
        """ Establish connection to database """
        parser = configparser.ConfigParser()
        parser.read(str(Path(getcwd()).parents[0]) + '/database.ini')
        db_creds = dict(parser.items(parser.sections()[0]))
        sql_url = 'dialect://user:password@host:port/database'
        for el in db_creds: sql_url = sql_url.replace(el, db_creds[el])
        
        return create_engine(sql_url)
        
    
    def nba_api(self, module, postgres):
        """ Call files in directory 'nba_api_calls' """
        
        mod = SourceFileLoader('get_' + module, 'nba_api_calls/' + module + '.py').load_module()
        func = getattr(mod, 'get_' + module)
        func(postgres)
        
          
    def fty_api_cxn(self):
        """ Create connection object to fanstasy api """
        
        parser = configparser.ConfigParser()
        parser.read(str(Path(getcwd()).parents[0]) + '/database.ini')
        fty_creds = dict(parser.items(parser.sections()[1]))
        
        return bb.League(
            league_id=int(fty_creds['league_id']),
            year=int(fty_creds['year']),
            espn_s2=fty_creds['espn_s2'], 
            swid=fty_creds['swid']
        )
    
    def fty_get_free_agents(self, players, postgres):
        
        df = []
        for player in players:
            df.append({
                'player_id': player.playerId,
                'player_name': player.name,
                'player_team': player.proTeam,
                'player_status': player.injuryStatus,
                'player_position': player.position
            }) 

        # Clean in prep for database ingestion
        df = DataFrame(df)
        df['player_id'] = df['player_id'].astype('str')
        
        # Write to database
        df.to_sql('free_agents', postgres, schema='fty', index=False, if_exists='replace')
        print('free_agents has been updated')
    
    
        
        