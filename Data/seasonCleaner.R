library(tidyverse)
library(caret)
library(xgboost)
library(data.table)
library(zoo)

`%ni%` <- Negate(`%in%`)

rolling_avg_func <- function(x, window_size) {
  rollapply(x, list(seq(-window_size, -1)), FUN = mean, fill = NA, align = "left")
}

ggplot_missing <- function(x){
  if(!require(reshape2)){warning('you need to install reshape2')}
  require(reshape2)
  require(ggplot2)
  #### This function produces a plot of the missing data pattern
  #### in x. It is a modified version of a function in the 'neato' package
  x %>%
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, vjust=0.5)) +
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}

wdPath = 'C:/Users/admin/Desktop/Stat 656/repo/STAT656Project'
setwd(wdPath)

filename = 'game_data.RData'
load(filename)

# For reference
# filename = 'clean_events.RData'
# load(filename)
# team_ids = master_clean %>%
#   distinct(homeID, homeTeam)
filename = 'team_ids.RData'
# save(team_ids, file = filename)
load(filename)

# Fix various data errors
game_stats_fix = game_stats %>%
  filter(!is.na(season), !is.na(opp)) %>%
  relocate(season, gameID, gameDate) %>%
  mutate(gameDate = as.Date(gameDate)) %>%
  arrange(gameDate) %>%
  mutate(
    season   = as.factor(season), # convert numeric factors to factors for future processing
    gameID   = as.factor(gameID),
    gameType = as.factor(gameType), 
    team     = as.factor(team),
    opp      = as.factor(opp),
    across(contains('_R', ignore.case = FALSE), ~replace_na(.,.5)), # fix rates where neither team recorded a rate event 
    across(contains(c('_PP', '_PK', '_EN', '_EV')), ~replace_na(., 0)) # fix games where a team did not record time at specific strength
  )
    

ggplot_missing(game_stats_fix)

# check how many games we have from each season
season_counts = game_stats_fix %>%
  group_by(season) %>%
  summarize(gameCount = n())

train_p = .5

stats_train = game_stats_fix %>%
  group_by(season) %>%
  filter(gameType == 2) %>% # limit to regular season
  filter(row_number() <= train_p * n()) %>%
  group_by(season, team) %>% # not sure why this is needed, but apparently it is
  summarise(across(where(is.numeric), mean))


test_split = game_stats_fix %>%
  group_by(season) %>%
  filter(gameType == 2) %>% 
  filter(row_number() > train_p * n()) %>%
  filter(home == TRUE) %>% # only analyze home games to look at each game once
  select(gameID, team, opp, goals, opp_goals) %>%
  rename(homeTeam = team, awayTeam = opp, homeGoals = goals, awayGoals = opp_goals) %>%
  mutate(win = ifelse(homeGoals > awayGoals, 1, 0)) %>%
  relocate(gameID, season, homeTeam, awayTeam, win) %>%
  # merge team stats (use left join with many to one relationship to get each team's values for the season)
  left_join(stats_train, join_by(season, homeTeam == team), 
            relationship = 'many-to-one', suffix = c('', '.home')) %>% 
  left_join(stats_train, join_by(season, awayTeam == team), 
            relationship = 'many-to-one', suffix = c('.home', '.away')) # match opp to team to get opposing team stats



# check how many games we have from each season
season_counts = test_split %>%
  group_by(season) %>%
  summarize(gameCount = n())
  

# test_roll = game_stats_fix %>%
#   group_by(season, team) %>%
#   mutate(across(where(is.numeric), 
#                 ~rollapply(.,
#                            width = as.list(-seq(1:25)), 
#                            mean,
#                            fill = NA, 
#                            partial = TRUE))) %>%
#   filter(row_number() > 25) 


test_roll = game_stats_fix %>%
  group_by(season, team) %>%
  mutate(across(where(is.numeric), 
                rolling_avg_func(., window_size = 25))) %>%
  filter(row_number() > 25) 


roll25 = test_roll %>%
  arrange(desc(home)) %>%
  left_join(test_roll, join_by(season, gameID, gameDate, gameType, team == opp, opp == team),
            relationship = 'one-to-one', suffix = c('.home', '.away')) %>%
  filter(home.home == TRUE) %>%
  select(-home.away) %>%
  rename(home = home.home) %>%
  drop_na()

ggplot_missing(roll25)

filename = 'split_games.RData'
save(test_split, file = filename)
filename = 'roll_25games.RData'
save(roll25, file = filename)

