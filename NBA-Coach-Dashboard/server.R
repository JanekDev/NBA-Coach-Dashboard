library(shiny)
library(fmsb)
library(nbastatR)

AVAILABLE_SEASONS =  c(2023)

STATS_COLS = c( "trbPerGame", "astPerGame", "stlPerGame", "blkPerGame", "ptsPerGame")
RATING_COL = c("ratioPER")

getPlayersDataFrame <- function (seasons){
  players_stats = bref_players_stats(
    tables = c("advanced", "per_game"),
    seasons = seasons,
    assign_to_environment = FALSE
  )
  relevant_cols = c("namePlayer","minutes", "slugPosition", "agePlayer", "slugTeamBREF", "countGames", "isHOFPlayer", "slugTeamsBREF", "urlPlayerHeadshot", "ratioPER", "trbPerGame", "astPerGame", "stlPerGame", "blkPerGame", "ptsPerGame")
  players_stats = players_stats[players_stats$minutes > 144,relevant_cols]
}
  
getBestStats <- function(players_df, position=NULL){
  if(position){
    players_df %<>% filter(slugPosition==position)
  }
  return(apply(players_df[,STATS_COLS], 2, function(x) max(x, na.rm = TRUE)))
}

getBestPlayer <- function(players_df, team, position){
  filter(players_df, )
}

player_stat_spider_plot <- function(player_stats){
  
}

function(input, output, session) {
  
  players_df = getPlayersDataFrame(seasons = tail(AVAILABLE_SEASONS, 1))

  output$select_season <- renderUI({
      selectInput(
        "select_season",
        "Select Season",
        AVAILABLE_SEASONS
      )
    })
  output$select_team <- renderUI({
      selectInput(
        "select_team",
        "Select Team",
        unique(players_df$slugTeamBREF)
      )
    })
  
  output$table <- renderTable({
    return(filter(players_df, slugTeamBREF==input$select_team))
  })
  
  output$starting_five_plot_PG <- getBestPlayer(position="PG", team=input$selected_team) %>% player_stats_spider_plot
  output$starting_five_plot_SG <- getBestPlayer(position="SG", team=input$selected_team) %>% player_stats_spider_plot
  output$starting_five_plot_PF <- getBestPlayer(position="PF", team=input$selected_team) %>% player_stats_spider_plot
  output$starting_five_plot_SF <- getBestPlayer(position="SF", team=input$selected_team) %>% player_stats_spider_plot
  output$starting_five_plot_C <- getBestPlayer(position="C", team=input$selected_team) %>% player_stats_spider_plot
}

