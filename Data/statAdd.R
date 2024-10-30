library(tidyverse)
library(caret)
library(xgboost)

`%ni%` <- Negate(`%in%`)

wdPath = 'C:/Users/admin/Desktop/Stat 656/repo/STAT656Project'
setwd(wdPath)

filename = 'clean_events.RData'
load(filename)

master_ids = master_clean %>% 
  mutate(ID = row_number())

# Split shots data
{
  shots = master_ids %>%
    mutate(shotOutcome = case_when(
      typeDescKey == 'shot-on-goal' ~ 'save',
      typeDescKey == 'blocked-shot' ~ 'block',
      typeDescKey == 'missed-shot'  ~ 'miss',
      typeDescKey == 'goal'         ~ 'goal',
      typeDescKey == 'failed-shot-attempt' ~ 'fail'
    )) %>%
    filter(grepl('shot', typeDescKey, ignore.case = TRUE) | typeDescKey == 'goal') %>%
    mutate(shotDistance = sqrt((89 - abs(xCoord))**2 + yCoord**2), 
           shotAngle = abs(atan(yCoord / (89 - abs(xCoord))) * 180 / pi),
           isGoal = ifelse(shotOutcome == 'goal', 1, 0))
  }

#### XGBoost ####
filename = 'xgModel_EV'
xg_EV = xgb.load(filename)
filename = 'xgModel_PP'
xg_PP = xgb.load(filename)
filename = 'xgModel_PK'
xg_PK = xgb.load(filename)
filename = 'xgModel_EN'
xg_EN = xgb.load(filename)
filename = 'xgModel_PS'
xg_PS = xgb.load(filename)

filename = 'xgBoost_EV.RData'
load(filename)



test = xgBoost_EV[1,] %>%
  select(-season, -isGoal) %>% as.matrix()
predict(xg_EV, newdata = test)

# The PLAN
# Need to remember that we did not include blocked shots (probably explains increased performance)
# split out shots like we did for training, then calculate xG values
# once I've got xG values, remerge using id values of shots (rows should still be in original order)



# Clean shots
{ 
  shots_clean = shots %>%
    select(-eventId, -timeInPeriod, -timeRemaining, -typeCode, -homeTeam, -homeID,
           -awayTeam, -awayID, -date, -homeTeamDefendingSide, -periodType, 
           -eventOwnerTeamId, -losingPlayerId, -winningPlayerId, -scoringPlayerId, 
           -scoringPlayerTotal, -assist1PlayerId, -assist1PlayerTotal, 
           -goalieInNetId, -playerId, -hittingPlayerId, -hitteePlayerId,
           -blockingPlayerId, -shootingPlayerId, -assist2PlayerId, -reason,
           -assist2PlayerTotal, -awaySOG, -homeSOG, -secondaryReason, -typeCode.1,
           -descKey, -duration, -committedByPlayerId, -servedByPlayerId, -bugCheck,
           -bugR, -sitReverse, -goalies, -drawnByPlayerId, -otPeriods, -sortOrder,
           -shotOutcome) %>%
    mutate(
      across(c(prev_dist_x_2, prev_dist_y_2, prev_distance_2, prev_time_2), ~ 
               if_else(prev_event_2 == 'stoppage', 0, .)),
      across(c(prev_home_event_2, prev_zone_2), ~
               if_else(prev_event_2 == 'stoppage', 
                       get(sub('_2', '', cur_column())), .)
      ),
      goalieExist = ifelse(homeEvent == 1,
                           awayGoalie, homeGoalie),
      stateDesc = ifelse(homeEvent == 1, 
                         paste(homeSkaters, 'v', awaySkaters, 
                               homeGoalie, awayGoalie, sep = ' '),
                         paste(awaySkaters, 'v', homeSkaters, 
                               awayGoalie, homeGoalie, sep = ' ')),
      scoreDiff = ifelse(homeEvent == 1, homeScore-awayScore, awayScore-homeScore),
      turnover = ifelse(homeEvent == prev_home_event, 0, 1),
      turnover_prev = ifelse(prev_home_event == prev_home_event_2, 0, 1)
    ) %>%
    filter(typeDescKey != 'blocked-shot') %>%
    select(-c(homeScore, awayScore, gameID, typeDescKey, homeSkaters, 
              awaySkaters, homeGoalie, awayGoalie, zoneCode))
  
  
  cats = shots_clean %>% 
    select(gameType, number, prev_event, prev_event_2, 
           prev_zone, prev_zone_2, skaters, str_state, stateDesc, shotType) %>%
    mutate(gameType = as.factor(gameType), number = as.factor(number),
           skaters = as.factor(skaters))
  dumm <- dummyVars(' ~ .', data = cats)
  cats_encoded = data.frame(predict(dumm, newdata = cats))
  
  nums = shots_clean %>%
    select(-c(gameType, number, prev_event, prev_event_2, 
              prev_zone, prev_zone_2, skaters, 
              netEmpty, str_state, stateDesc, shotType))
  
  xgBoost_data_full = cbind(nums, cats_encoded)
  xgBoost_data = xgBoost_data_full %>% filter(prev_eventstoppage == 0) %>%
    select(-prev_eventstoppage)
  
  xgBoost_EV = xgBoost_data %>% 
    filter(str_stateEV == 1)
  xgBoost_EV_id = xgBoost_EV$ID
  xgBoost_EV_in = xgBoost_EV %>% 
    select(-ID, -season, -isGoal) %>%
    as.matrix()
  
  xgBoost_PP = xgBoost_data %>%
    filter(str_statePP == 1)
  xgBoost_PP_id = xgBoost_PP$ID
  xgBoost_PP_in = xgBoost_PP %>% 
    select(-ID, -season, -isGoal) %>%
    as.matrix()
  
  xgBoost_PK = xgBoost_data %>%
    filter(str_statePK == 1)
  xgBoost_PK_id = xgBoost_PK$ID
  xgBoost_PK_in = xgBoost_PK %>% 
    select(-ID, -season, -isGoal) %>%
    as.matrix()
  
  xgBoost_EN = xgBoost_data %>%
    filter(str_stateEN == 1 | str_stateNG == 1)
  xgBoost_EN_id = xgBoost_EN$ID
  xgBoost_EN_in = xgBoost_EN %>% 
    select(-ID, -season, -isGoal) %>%
    as.matrix()
  
  xgBoost_PS = xgBoost_data %>%
    filter(str_statePS == 1) 
  xgBoost_PS_id = xgBoost_PS$ID
  xgBoost_PS_in = xgBoost_PS %>% 
    select(-ID, -season, -isGoal) %>%
    as.matrix()
}

# Plug into xG models
{
  xgBoost_EV_out = predict(xg_EV, newdata = xgBoost_EV_in)
  xgBoost_EV_index = data.frame(ID = xgBoost_EV_id, xg = xgBoost_EV_out)
  
  xgBoost_PP_out = predict(xg_PP, newdata = xgBoost_PP_in)
  xgBoost_PP_index = data.frame(ID = xgBoost_PP_id, xg = xgBoost_PP_out)
  
  xgBoost_PK_out = predict(xg_PK, newdata = xgBoost_PK_in)
  xgBoost_PK_index = data.frame(ID = xgBoost_PK_id, xg = xgBoost_PK_out)
  
  xgBoost_EN_out = predict(xg_EN, newdata = xgBoost_EN_in)
  xgBoost_EN_index = data.frame(ID = xgBoost_EN_id, xg = xgBoost_EN_out)
  
  xgBoost_PS_out = predict(xg_PS, newdata = xgBoost_PS_in)
  xgBoost_PS_index = data.frame(ID = xgBoost_PS_id, xg = xgBoost_PS_out)
  
  xgBoost_full_index = rbind(xgBoost_EV_index, xgBoost_PP_index, xgBoost_PK_index,
                             xgBoost_EN_index, xgBoost_PS_index)
}




#### Logistic ####
# load model
filename = 'glm_logistic_xg_V1.rds'
log_xg = readRDS(filename)

# Clean data
{
  shot <- shots %>%
    mutate(awayScore = lag(awayScore),
           homeScore = lag(homeScore))
  
  ######  Look into missing values...
  colnames(shot)[colSums(is.na(shot)) == 0]
  colSums(is.na(shot))
  
  
  #found NA's for lots of rows, some of them came from SO's, well drop these, and Drop some unnecessary variables
  #also Lets drop the situations where XCoord, Ycoord, or homeevent are missing. These must be missing and they are essential
  shots_modelprep = shot %>% filter(periodType != 'SO') %>% 
    select(-c( timeInPeriod, homeTeam, awayTeam, homeID, awayID, date, 
               homeTeamDefendingSide, sortOrder, typeCode, eventOwnerTeamId, losingPlayerId, 
               winningPlayerId, reason, shootingPlayerId, goalieInNetId, awaySOG, homeSOG, typeCode.1, 
               timeSec, blockingPlayerId, descKey, duration, committedByPlayerId, drawnByPlayerId, playerId, 
               secondaryReason, scoringPlayerId, scoringPlayerTotal, assist1PlayerId, assist1PlayerTotal, 
               assist2PlayerId, assist2PlayerTotal, hittingPlayerId, hitteePlayerId, servedByPlayerId,
               season, gameID, gameType, bugR, bugCheck, sitReverse, awayGoalie, awaySkaters,
               homeGoalie, homeSkaters, shotOutcome, timeRemaining, gameSec, otPeriods, timeRemaining))  %>% 
    filter(is.na(awayScore) == 0 & is.na(homeScore) == 0 & is.na(xCoord) == 0 & is.na(yCoord) == 0 & is.na(homeEvent) == 0)
  
  
  #This seemed to take out a lot of NA's. Still have a lot left, and most seem to deal with prev_dist/event
  #seems like alot have to do with stoppages. there should never be a stoppage, gameend, period end as the last play, it should be a faceoff
  #these situations will not have a previous location, this is important in our analysis, so we will drop these
  shots_modelprep1 = shots_modelprep %>% filter(!(prev_event %in% c('stoppage', 'game-end', 'period-end')))
  
  #if the prev_dist_x is NA, and the prev_event was blocked-shot, hit, SOG, or takeaway, drop these
  shots_modelprep2 = shots_modelprep1 %>%
    filter(!(is.na(prev_dist_x) & prev_event %in% c("blocked-shot", "hit", "shot-on-goal", "takeaway")))
  
  #now we see there are two situations left where the prev_dist_x is NA (delayed-penalty and penalty (~missing 5% of time))
  #I will drop these 
  shots_modelprep3 = shots_modelprep2 %>% filter(!(is.na(prev_dist_x) & prev_event %in% c("penalty", "delayed-penalty")))
  
  #now we see we have a lot of situations where there is no prev_dist_x and its due to period-end/start, game-end.
  #in these situations, we should have the prev_location set as the center ice faceoff dot
  shots_modelprep3$prev_dist_x[is.na(shots_modelprep3$prev_dist_x)] <- shots_modelprep3$xCoord[is.na(shots_modelprep3$prev_dist_x)]
  shots_modelprep3$prev_dist_y[is.na(shots_modelprep3$prev_dist_y)] <- shots_modelprep3$yCoord[is.na(shots_modelprep3$prev_dist_y)]
  shots_modelprep3$prev_distance[is.na(shots_modelprep3$prev_distance)] <- sqrt((shots_modelprep3$xCoord[is.na(shots_modelprep3$prev_distance)])^2 + (shots_modelprep3$yCoord[is.na(shots_modelprep3$prev_distance)])^2)
  
  
  #now if prev_event_2 was a stoppage, or prev_event was a faceoff, or period-end, or period-start set its distance from b4 that equal to 0
  shots_modelprep4 = shots_modelprep3 %>% 
    mutate(prev_dist_x_2 = ifelse(is.na(prev_dist_x_2) & (prev_event_2 == "stoppage" | prev_event %in% c("faceoff", "period-end", "period-start", "game-end")), 0, prev_dist_x_2),
           prev_dist_y_2 = ifelse(is.na(prev_dist_y_2) & (prev_event_2 == "stoppage"| prev_event %in% c("faceoff", "period-end", "period-start", "game-end")), 0, prev_dist_y_2),
           prev_distance_2 = ifelse(is.na(prev_distance_2) & (prev_event_2 == "stoppage" | prev_event %in% c("faceoff", "period-end", "period-start", "game-end")), 0, prev_distance_2))
  
  
  #set the rest of prev distances_2 equal to 0, We will drop the rows where it was a blocked shot with missing prev_zone_2 and then set all the rest to none for prev_zone_2, prev_home_event set variable types
  final_modelprep = shots_modelprep4 %>% 
    mutate(prev_dist_x_2 = ifelse(is.na(prev_dist_x_2), 0, prev_dist_x_2),
           prev_dist_y_2 = ifelse(is.na(prev_dist_y_2), 0, prev_dist_y_2),
           prev_distance_2 = ifelse(is.na(prev_distance_2), 0, prev_distance_2)) %>% 
    filter((is.na(prev_zone_2) == 0) | ((is.na(prev_zone_2) > 0) & (prev_event_2 != 'blocked-shot'))) %>% 
    mutate(prev_zone_2 = ifelse(is.na(prev_zone_2), '', prev_zone_2), 
           prev_home_event_2 = ifelse(is.na(prev_home_event_2), 'none', prev_home_event_2)) %>% 
    filter((is.na(prev_home_event) == 0) | ((is.na(prev_home_event) > 0) & (prev_event != 'blocked-shot'))) %>% 
    mutate(prev_home_event = ifelse(is.na(prev_home_event), 'none', prev_home_event)) %>%
    mutate(shotAngle = ifelse(is.na(shotAngle), 0, shotAngle)) %>% 
    select(-typeDescKey) %>% 
    mutate(periodType = as.factor(periodType),
           zoneCode = as.factor(zoneCode),
           shotType = as.factor(shotType),
           periodSec = as.numeric(periodSec),
           prev_event = as.factor(prev_event),
           prev_event_2 = as.factor(prev_event_2),
           prev_time= as.numeric(prev_time),
           prev_time_2 = as.numeric(prev_time_2),
           prev_home_event = as.factor(prev_home_event),
           prev_home_event_2 = as.factor(prev_home_event_2),
           prev_zone = as.factor(prev_zone),
           prev_zone_2 = as.factor(prev_zone_2),
           str_state = as.factor(str_state),
           homeEvent = as.factor(homeEvent),
           number = as.factor(number)
    ) 
  
  #Check missing values and clear workspace
  colSums(is.na(final_modelprep))
  str(final_modelprep)
  rm(shots_modelprep)
  rm(shots_modelprep1)
  rm(shots_modelprep2)
  rm(shots_modelprep3)
  rm(shots)
  rm(shot)
  
  log_ID  = select(final_modelprep, ID)
  Y       = select(final_modelprep, isGoal ) %>% unlist()
  X       = select(final_modelprep,-c(isGoal, eventId, goalies, skaters, netEmpty, xCoord, yCoord, -ID)) #netEmpty included in str_state, xCoord/yCoord very non-linear
  
  Xqual       = X %>% select(c(periodType, zoneCode, shotType, prev_event, prev_event_2, prev_home_event, prev_home_event_2,
                               prev_zone, prev_zone_2, str_state, homeEvent, number))
  Xquan = X %>% select(-one_of(colnames(Xqual)))
  
  dummyModel = dummyVars(~ ., data = Xqual, fullRank = TRUE)
  XqualDummy = predict(dummyModel, Xqual)
  combinedDF = cbind(Xquan, XqualDummy,Y) 
  X_df = combinedDF %>% select(-Y)
}

# Get predictions
log_xg_out = predict(log_xg, X_df, type = 'response')
log_xg_ids = data.frame(ID = log_ID$ID, log_xg = log_xg_out)


#### Merge xG and add stats ####
# Merge xgboost data with full event data
master_xgboost = merge(master_ids, xgBoost_full_index, all = TRUE, by = 'ID')
# merge log data with full event data
master_xg = merge(master_xgboost, log_xg_ids, all = TRUE, by = 'ID')

# Add markers
corsi_events   = c('save', 'miss', 'block', 'goal', 'fail')
fenwick_events = c('save', 'miss', 'goal')
sog_events     = c('save', 'goal')
master_processed = master_xg %>%
  mutate(
    corsi   = ifelse(shotOutcome %in% corsi_events, 1, 0),
    fenwick = ifelse(shotOutcome %in% fenwick_events, 1, 0),
    sog     = ifelse(shotOutcome %in% sog_events, 1, 0), 
    opp     = ifelse(homeEvent == 1, awayID, homeID)
  ) %>%
  filter(typeDescKey %ni% c('stoppage', 'period-start', 'period-end', 'game-end'))

# Get stats for and against by game
game_for = master_processed %>%
  group_by(gameID, eventOwnerTeamId) %>%
  summarise(
    season     = first(season),
    gameDate   = first(date),
    gameType   = first(gameType),
    team       = first(eventOwnerTeamId),
    home       = team == first(homeID),
    logit_xg   = sum(log_xg, na.rm = TRUE),
    xgboost_xg = sum(xg, na.rm = TRUE),
    goals      = sum(typeDescKey == 'goal'),
    CF         = sum(corsi),
    FF         = sum(fenwick), 
    shots      = sum(sog), 
    hits       = sum(typeDescKey == 'hit'),
    HDCF       = sum(((ifelse(is.na(xg), 0, xg) + ifelse(is.na(log_xg), 0, log_xg))/2) > .14),
    Penalties  = sum(typeDescKey == 'penalty'), 
    faceoff_W  = sum(typeDescKey == 'faceoff')
  )
game_opp = master_processed %>%
  group_by(gameID, opp) %>%
  summarise(
    opp_id         = first(eventOwnerTeamId),
    opp_logit_xg   = sum(log_xg, na.rm = TRUE),
    opp_xgboost_xg = sum(xg, na.rm = TRUE),
    opp_goals      = sum(typeDescKey == 'goal'),
    opp_CF         = sum(corsi),
    opp_FF         = sum(fenwick), 
    opp_shots      = sum(sog), 
    opp_hits       = sum(typeDescKey == 'hit'),
    opp_HDCF       = sum(((ifelse(is.na(xg), 0, xg) + ifelse(is.na(log_xg), 0, log_xg))/2) > .14),
    opp_Penalties  = sum(typeDescKey == 'penalty'),
    opp_faceoff_W  = sum(typeDescKey == 'faceoff')
  )
 
# EV
game_EV_for = master_processed %>%
  filter(str_state == 'EV') %>%
  group_by(gameID, eventOwnerTeamId) %>%
  summarise(
    logit_xg_EV   = sum(log_xg, na.rm = TRUE),
    xgboost_xg_EV = sum(xg, na.rm = TRUE),
    goals_EV      = sum(typeDescKey == 'goal'),
    CF_EV         = sum(corsi),
    FF_EV         = sum(fenwick), 
    shots_EV      = sum(sog), 
    hits_EV       = sum(typeDescKey == 'hit'),
    HDCF_EV       = sum(((ifelse(is.na(xg), 0, xg) + ifelse(is.na(log_xg), 0, log_xg))/2) > .14)
  )
game_EV_opp = master_processed %>%
  filter(str_state == 'EV') %>%
  group_by(gameID, opp) %>%
  summarise(
    opp_logit_xg_EV   = sum(log_xg, na.rm = TRUE),
    opp_xgboost_xg_EV = sum(xg, na.rm = TRUE),
    opp_goals_EV      = sum(typeDescKey == 'goal'),
    opp_CF_EV         = sum(corsi),
    opp_FF_EV         = sum(fenwick), 
    opp_shots_EV      = sum(sog), 
    opp_hits_EV       = sum(typeDescKey == 'hit'),
    opp_HDCF_EV       = sum(((ifelse(is.na(xg), 0, xg) + ifelse(is.na(log_xg), 0, log_xg))/2) > .14)
  )
  
# PP
game_PP_for = master_processed %>%
  filter(str_state == 'PP') %>%
  group_by(gameID, eventOwnerTeamId) %>%
  summarise(
    logit_xg_PP   = sum(log_xg, na.rm = TRUE),
    xgboost_xg_PP = sum(xg, na.rm = TRUE),
    goals_PP      = sum(typeDescKey == 'goal'),
    CF_PP         = sum(corsi),
    FF_PP         = sum(fenwick), 
    shots_PP      = sum(sog), 
    hits_PP       = sum(typeDescKey == 'hit'),
    HDCF_PP       = sum(((ifelse(is.na(xg), 0, xg) + ifelse(is.na(log_xg), 0, log_xg))/2) > .14)
  )
game_PP_opp = master_processed %>%
  filter(str_state == 'PP') %>%
  group_by(gameID, opp) %>%
  summarise(
    opp_logit_xg_PP   = sum(log_xg, na.rm = TRUE),
    opp_xgboost_xg_PP = sum(xg, na.rm = TRUE),
    opp_goals_PP      = sum(typeDescKey == 'goal'),
    opp_CF_PP         = sum(corsi),
    opp_FF_PP         = sum(fenwick), 
    opp_shots_PP      = sum(sog), 
    opp_hits_PP       = sum(typeDescKey == 'hit'),
    opp_HDCF_PP       = sum(((ifelse(is.na(xg), 0, xg) + ifelse(is.na(log_xg), 0, log_xg))/2) > .14)
  )

# PK
game_PK_for = master_processed %>%
  filter(str_state == 'PK') %>%
  group_by(gameID, eventOwnerTeamId) %>%
  summarise(
    logit_xg_PK   = sum(log_xg, na.rm = TRUE),
    xgboost_xg_PK = sum(xg, na.rm = TRUE),
    goals_PK      = sum(typeDescKey == 'goal'),
    CF_PK         = sum(corsi),
    FF_PK         = sum(fenwick), 
    shots_PK      = sum(sog), 
    hits_PK       = sum(typeDescKey == 'hit'),
    HDCF_PK       = sum(((ifelse(is.na(xg), 0, xg) + ifelse(is.na(log_xg), 0, log_xg))/2) > .14)
  )
game_PK_opp = master_processed %>%
  filter(str_state == 'PK') %>%
group_by(gameID, opp) %>%
  summarise(
    opp_logit_xg_PK   = sum(log_xg, na.rm = TRUE),
    opp_xgboost_xg_PK = sum(xg, na.rm = TRUE),
    opp_goals_PK      = sum(typeDescKey == 'goal'),
    opp_CF_PK         = sum(corsi),
    opp_FF_PK         = sum(fenwick), 
    opp_shots_PK      = sum(sog), 
    opp_hits_PK       = sum(typeDescKey == 'hit'),
    opp_HDCF_PK       = sum(((ifelse(is.na(xg), 0, xg) + ifelse(is.na(log_xg), 0, log_xg))/2) > .14)
  )

# combine stats and add additional info
game_stats = game_for %>% merge(game_opp, by.x = c('gameID', 'team'), by.y = c('gameID', 'opp'), all = TRUE) %>%
  merge(game_EV_for, by   = c('gameID', 'eventOwnerTeamId'), all.x = TRUE) %>%
  merge(game_EV_opp, by.x = c('gameID', 'team'), by.y = c('gameID', 'opp'), all.x = TRUE) %>%
  merge(game_PP_for, by   = c('gameID', 'eventOwnerTeamId'), all.x = TRUE) %>%
  merge(game_PP_opp, by.x = c('gameID', 'team'), by.y = c('gameID', 'opp'), all.x = TRUE) %>%
  merge(game_PK_for, by   = c('gameID', 'eventOwnerTeamId'), all.x = TRUE) %>%
  merge(game_PK_opp, by.x = c('gameID', 'team'), by.y = c('gameID', 'opp'), all.x = TRUE) %>%
  select(-eventOwnerTeamId) %>%
  mutate(
    logit_xg_R   = logit_xg / (logit_xg + opp_logit_xg),
    xgboost_xg_R = xgboost_xg / (xgboost_xg + opp_xgboost_xg),
    CF_R         = CF / (CF + opp_CF), 
    FF_R         = FF / (FF + opp_FF), 
    HDCF_R       = HDCF / (HDCF + opp_HDCF),
    FO_R         = faceoff_W / (faceoff_W + opp_faceoff_W),
    # EV stats
    logit_xg_EV_R   = logit_xg_EV / (logit_xg_EV + opp_logit_xg_EV), 
    xgboost_xg_EV_R = xgboost_xg_EV / (xgboost_xg_EV + opp_xgboost_xg_EV),
    CF_EV_R         = CF_EV / (CF_EV + opp_CF_EV),
    FF_EV_R         = FF_EV / (FF_EV + opp_FF_EV),
    HDCF_EV_R       = HDCF_EV / (HDCF_EV + opp_HDCF_EV)
    ) %>%
  relocate(season, gameID, gameType, team, opp_id, home, goals, opp_goals) %>%
  rename(opp = 'opp_id') %>%
  filter(!is.na(team))

#### Save Output ####
filename = 'game_data.RData'
save(game_stats, file = filename)
