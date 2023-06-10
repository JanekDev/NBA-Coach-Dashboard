library(nbastatR)
team_data <- team_season_roster(team = "Denver Nuggets", season = 2023)

team_data <- team_data[order(team_data$age)
# Get the top 5 players
team_data <- head(team_data, 5)
teams = nba_teams()
teams_dates = nba_tea