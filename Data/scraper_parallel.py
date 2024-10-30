import pandas as pd
from selenium import webdriver
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from webdriver_manager.chrome import ChromeDriverManager
from selenium.common.exceptions import TimeoutException
import json
from dateutil import parser
import datetime
import multiprocessing


# Get Team Abbrevs
teams = ['NJD', 'DET', 'BOS', 'WPG', 'SJS', 'PIT', 'TBL', 'PHI', 'TOR', 'BUF', 'CAR', 'ARI', 'CGY', 'MTL', 'WSH',
         'VAN',
         'COL', 'NSH', 'ANA', 'VGK', 'SEA', 'DAL', 'CHI', 'NYR', 'CBJ', 'FLA', 'EDM', 'MIN', 'STL', 'OTT', 'NYI',
         'LAK']

# Get each team's games for each season
seasons = ['20152016', '20162017', '20172018', '20182019', '20192020', '20212022', '20222023', '20232024']


def get_team_games(team):
    seasons = ['20152016', '20162017', '20172018', '20182019', '20192020', '20212022', '20222023', '20232024']
    game_events = []
    chrome_options = webdriver.ChromeOptions()
    driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=chrome_options)
    for season in seasons:
        try:
            # Scrape season schedule
            url = f'https://api-web.nhle.com/v1/club-schedule-season/{team}/{season}'
            driver.get(url)
            games_txt = driver.find_element(By.TAG_NAME, 'body').text
            games_data = json.loads(games_txt)['games']
            for game in games_data:
                # Get basic game data
                game_id = game['id']
                game_date = parser.parse(game['gameDate']).date()
                if game_date < datetime.date.today():
                    try:
                        # Scrape game events
                        url_game = f'https://api-web.nhle.com/v1/gamecenter/{game_id}/play-by-play'
                        driver.get(url_game)
                        events_txt = driver.find_element(By.TAG_NAME, 'body').text
                        events_data = json.loads(events_txt)
                        home = events_data['homeTeam']
                        away = events_data['awayTeam']
                        id_game = events_data['id']
                        game_type = events_data['gameType']
                        # Add game info to events
                        for item in events_data['plays']:
                            item['homeTeam'] = home['abbrev']
                            item['homeID'] = home['id']
                            item['awayTeam'] = away['abbrev']
                            item['awayID'] = away['id']
                            item['date'] = game_date
                            item['gameID'] = id_game
                            item['season'] = season
                            item['gameType'] = game_type
                        game_events.extend(events_data['plays'])
                    except Exception as e:
                        print(f'Error fetching data for game {game["id"]} : {str(e)}')
        except Exception as e:
            print(f"Error fetching data for team {team} in season {season}: {str(e)}")
    driver.quit()
    return game_events

if __name__ == '__main__':

    pool = multiprocessing.Pool(processes=8)
    results = pool.map(get_team_games, teams)
    pool.close()
    pool.join()

    for i in range(len(teams)):
        df_raw = pd.DataFrame.from_dict(results[i], orient='columns')
        df_details = pd.json_normalize(df_raw['details'])
        df_period = pd.json_normalize(df_raw['periodDescriptor'])
        df_full = pd.concat([df_raw, df_period, df_details], axis=1)
        df_full = df_full.drop(['details', 'periodDescriptor'], axis=1)
        id_cols = [col for col in df_full.columns if 'Id' in col]
        for col in id_cols:
            df_full[col] = df_full[col].astype('Int64')
        filename = teams[i] + '20152024.csv'
        df_full.to_csv(filename)

    'https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId=2021020001'
    'https://api-web.nhle.com/v1/roster/TBL/20232024'
    'https://api.nhle.com/stats/rest/en/team'

