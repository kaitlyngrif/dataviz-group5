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
from concurrent.futures import ThreadPoolExecutor


chrome_options = webdriver.ChromeOptions()
driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=chrome_options)
# Get Team Abbrevs
teams = ['NJD', 'DET', 'BOS', 'WPG', 'SJS', 'PIT', 'TBL', 'PHI', 'TOR', 'BUF', 'CAR', 'ARI', 'CGY', 'MTL', 'WSH', 'VAN',
         'COL', 'NSH', 'ANA', 'VGK', 'SEA', 'DAL', 'CHI', 'NYR', 'CBJ', 'FLA', 'EDM', 'MIN', 'STL', 'OTT', 'NYI', 'LAK']

# Get Player IDs
playerFirsts = []
playerLasts = []
playerIDs = []
teamAbbr = []
for team in teams:
    url = ''.join(['https://api-web.nhle.com/v1/roster/', str(team), '/20232024'])
    driver.get(url)
    rosterTxt = driver.find_element(By.TAG_NAME, 'body').text
    roster = json.loads(rosterTxt)
    playerFirsts.extend([p['firstName']['default'] for p in roster['forwards']])
    playerFirsts.extend([p['firstName']['default'] for p in roster['defensemen']])
    playerFirsts.extend([p['firstName']['default'] for p in roster['goalies']])
    playerLasts.extend([p['lastName']['default'] for p in roster['forwards']])
    playerLasts.extend([p['lastName']['default'] for p in roster['defensemen']])
    playerLasts.extend([p['lastName']['default'] for p in roster['goalies']])
    playerIDs.extend([p['id'] for p in roster['forwards']])
    playerIDs.extend([p['id'] for p in roster['defensemen']])
    playerIDs.extend([p['id'] for p in roster['goalies']])
    teamAbbr.extend(team for p in roster['forwards'])
    teamAbbr.extend(team for p in roster['defensemen'])
    teamAbbr.extend(team for p in roster['goalies'])

names = tuple(zip(playerFirsts, playerLasts))
players = {playerIDs[i] : [names[i], teamAbbr[i]] for i in range(len(playerIDs))}


# Get each team's games for each season
seasons = ['20152016', '20162017', '20172018', '20182019', '20192020', '20212022', '20222023', '20232024']

gamesByTeam = []
for team in teams:
    gameEvents = []
    for season in seasons:
        url = ''.join(['https://api-web.nhle.com/v1/club-schedule-season/', team, '/', season])
        driver.get(url)
        gamesTxt = driver.find_element(By.TAG_NAME, 'body').text
        games = json.loads(gamesTxt)['games']
        gamesID = [d['id'] for d in games]
        for i in gamesID:
            url = ''.join(['https://api-web.nhle.com/v1/gamecenter/', str(i), '/play-by-play'])
            driver.get(url)
            eventsTxt = driver.find_element(By.TAG_NAME, 'body').text
            date = parser.parse(json.loads(eventsTxt)['gameDate']).date()
            if date < datetime.date.today():
                events = json.loads(eventsTxt)['plays']
                home = json.loads(eventsTxt)['homeTeam']
                away = json.loads(eventsTxt)['awayTeam']
                id_game = json.loads(eventsTxt)['id']
                gameType = json.loads(eventsTxt)['gameType']
                for item in events:
                    item['homeTeam'] = home['abbrev']
                    item['homeID'] = home['id']
                    item['awayTeam'] = away['abbrev']
                    item['awayID'] = away['id']
                    item['date'] = date
                    item['gameID'] = id_game
                    item['season'] = season
                    item['gameType'] = gameType
                gameEvents.extend(events)
    gamesByTeam.append(gameEvents)


for i in range(len(teams)):
    df_raw = pd.DataFrame.from_dict(gamesByTeam[i], orient='columns')
    df_details = pd.json_normalize(df_raw['details'])
    df_period = pd.json_normalize(df_raw['periodDescriptor'])
    df_full = pd.concat([df_raw, df_period, df_details], axis=1)
    df_full = df_full.drop(['details', 'periodDescriptor'], axis=1)
    id_cols = [col for col in df_full.columns if 'Id' in col]

    for col in id_cols:
        df_full[col] = df_full[col].astype('Int64')

    filename = teams[i] + '.csv'
    df_full.to_csv(filename)


'https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId=2021020001'
'https://api-web.nhle.com/v1/roster/TBL/20232024'
'https://api.nhle.com/stats/rest/en/team'