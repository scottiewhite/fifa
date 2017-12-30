
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
leagueData <- dbReadTable(db, "League")
playerData <- dbReadTable(db, "Player")

players <- matchData %>%
  select(home_player_X1:away_player_11) %>%
  na.omit() %>%
  View()
  

#Pare down table and define results
matchData <- matchData %>%
  select(match_api_id:away_team_goal, B365H:LBA) %>%
  mutate(home_win =  ifelse(home_team_goal > away_team_goal, 1, 0)) %>%
  mutate(home_draw = ifelse(home_team_goal == away_team_goal, 1, 0)) %>%
  mutate(home_loss = ifelse(home_team_goal < away_team_goal, 1, 0)) %>%
  mutate(home_win_odds = 1/LBH) %>%
  mutate(home_draw_odds = 1/LBD) %>%
  mutate(home_loss_odds = 1/LBA) %>%
  mutate(vig = home_loss_odds + home_draw_odds + home_win_odds) %>%
  mutate(no_vig_home_win_odds = home_win_odds/vig) %>%
  mutate(no_vig_home_draw_odds = home_draw_odds/vig) %>%
  mutate(no_vig_home_loss_odds = home_loss_odds/vig) %>%
  mutate(index = 1)

# No Vigorish summary wins
no_vig_summary_wins <- matchData %>%
  group_by(no_vig_home_win_odds) %>%
  summarize(losses = sum(home_loss), 
            draws = sum(home_draw), 
            wins = sum(home_win), 
            games = sum(index))

# No Vigorish Wins
no_vig_summary_wins %>%
  filter(games>25) %>%
  ggplot(aes(x = no_vig_home_win_odds, y = wins/games, size = games)) + 
  geom_point() + 
  geom_abline(slope=1) + 
  geom_smooth(method = "lm") +
  xlab(label = "Pre-game implied home-team win probability") + 
  ylab(label = "Actual Home-team Win %") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)

# Vigorish summary wins
vig_summary_wins <- matchData %>%
  group_by(home_win_odds) %>%
  summarize(losses = sum(home_loss), 
            draws = sum(home_draw), 
            wins = sum(home_win), 
            games = sum(index))

# Vigorish Wins
vig_summary_wins %>%
  filter(games>25) %>%
  ggplot(aes(x = home_win_odds, y = wins/games, size = games)) + 
  geom_point() + 
  geom_abline(slope=1) + 
  geom_smooth(method = "lm") +
  xlab(label = "Pre-game implied home-team win probability") + 
  ylab(label = "Actual Home-team Win %") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)

# Vigorish Summary draws
vig_summary_draws <- matchData %>%
  group_by(home_draw_odds) %>%
  summarize(losses = sum(home_loss), 
            draws = sum(home_draw), 
            wins = sum(home_win), 
            games = sum(index))

# Vigorish Draws
vig_summary_draws %>%
  filter(games>25) %>%
  ggplot(aes(x = home_draw_odds, y = draws/games, size = games)) + 
  geom_point() + 
  geom_abline(slope=1) + 
  geom_smooth(method = "lm") +
  xlab(label = "Pre-game implied home-team draw probability") + 
  ylab(label = "Actual Home-team Draw %") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)


# No Vigorish Summary draws
no_vig_summary_draws <- matchData %>%
  group_by(no_vig_home_draw_odds) %>%
  summarize(losses = sum(home_loss), 
            draws = sum(home_draw), 
            wins = sum(home_win), 
            games = sum(index))

# No Vigorish Draws
no_vig_summary_draws %>%
  filter(games>25) %>%
  ggplot(aes(x = no_vig_home_draw_odds, y = draws/games, size = games)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  geom_abline(slope=1) + 
  xlab(label = "Pre-game implied home-team draw probability") + 
  ylab(label = "Actual Home-team Draw %") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)
