
library(R.utils)
library(RSQLite)
library(ggplot2)
library(tidyverse)
library(scales)
# Unzip file
fif <- unzip("~/Desktop/soccer.zip")

# Set up the SQLite
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver, dbname = fif)

dbListTables(db)

#Initialize the tables
matchData <- dbReadTable(db, "Match")

#Pare down table and define results
matchData <- matchData %>%
  select(match_api_id:away_team_goal, B365H:LBA) %>%
  mutate(home_win =  ifelse(home_team_goal > away_team_goal, 1, 0)) %>%
  mutate(home_draw = ifelse(home_team_goal == away_team_goal, 1, 0)) %>%
  mutate(home_loss = ifelse(home_team_goal < away_team_goal, 1, 0)) %>%
  mutate(home_win_odds = 1/LBH) %>%
  mutate(home_draw_odds = 1/LBD) %>%
  mutate(home_loss_odds = 1/LBA) %>%
  mutate(index = 1)
  
# Summary wins
summary_wins <- matchData %>%
  group_by(home_win_odds) %>%
  summarize(losses = sum(home_loss), 
            draws = sum(home_draw), 
            wins = sum(home_win), 
            games = sum(index))

# Wins
summary_wins %>%
  filter(games>50) %>%
  ggplot(aes(x = home_win_odds, y = wins/games)) + 
  geom_point() + 
  geom_abline(slope=1) + 
  xlab(label = "Pre-game implied home-team win probability") + 
  ylab(label = "Actual Home-team Win %") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)


# Summary draws
summary_draws <- matchData %>%
  group_by(home_draw_odds) %>%
  summarize(losses = sum(home_loss), 
            draws = sum(home_draw), 
            wins = sum(home_win), 
            games = sum(index))

# Draws
summary_draws %>%
  filter(games>25) %>%
  ggplot(aes(x = home_draw_odds, y = draws/games)) + 
  geom_point() + 
  geom_abline(slope=1) + 
  xlab(label = "Pre-game implied home-team draw probability") + 
  ylab(label = "Actual Home-team Draw %") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)
