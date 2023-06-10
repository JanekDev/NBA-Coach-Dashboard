library(nbastatR)
team_data <- team_season_roster(team = "Denver Nuggets", season = 2023)

team_data <- team_data[order(team_data$age)
# Get the top 5 players
team_data <- head(team_data, 5)
teams = nba_teams()
teams_dates = nba_tea

library(fmsb)
players_stats[1,cols]
cols = c("ratioPER", "trbPerGame", "astPerGame", "stlPerGame", "blkPerGame", "ptsPerGame")

radarchart(rbind(rep(20,6) , rep(0,6) , players_stats[1,cols])  ,
            axistype=1,

            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 ,

            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,

            #custom labels
            vlcex=0.8
)
