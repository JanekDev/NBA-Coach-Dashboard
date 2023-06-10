library(shiny)
library(fmsb)
library(nbastatR)
library(dplyr)
library(magrittr)

AVAILABLE_SEASONS =  c(2023)

STATS_COLS = c("trbPerGame", "astPerGame", "stlPerGame", "blkPerGame", "ptsPerGame")
RATING_COL = c("ratioPER")

getPlayersDataFrame <- function (){
  players_stats = bref_players_stats(
    tables = c("advanced", "per_game"),
    seasons = 2023,
    assign_to_environment = FALSE
  )
  relevant_cols = c("namePlayer","minutes", "slugPosition", "agePlayer", "slugTeamBREF", "countGames", "isHOFPlayer", "slugTeamsBREF", "urlPlayerHeadshot", "ratioPER", "trbPerGame", "astPerGame", "stlPerGame", "blkPerGame", "ptsPerGame")
  return(players_stats[players_stats$minutes > 144,relevant_cols])
}
  
getBestStats <- function(players_df, team=NULL, position=NULL, stats_cols = STATS_COLS){
  if(!is.null(position)){
    players_df %<>% dplyr::filter(slugPosition==position)
  }
  if(!is.null(team)){
    players_df %<>% dplyr::filter(slugTeamsBREF==team)
  }
  return(apply(players_df[,stats_cols], 2, function(x) max(x, na.rm = TRUE)))
}

getBestPlayer <- function(players_df, team=NULL, position=NULL, stat_col = RATING_COL){
  if(!is.null(team)){
    players_df %<>% dplyr::filter(slugTeamsBREF==team)
  }
  if(!is.null(position)){
    players_df %<>% dplyr::filter(slugPosition==position)
  }
  mask = players_df[,stat_col] == getBestStats(players_df, team =team, position = position, stats_cols = stat_col)
  return(players_df[mask,])
}

player_stat_spider_plot <- function(player, stats_cols){
  
  
}

function(input, output, session) {
  
  players_df = getPlayersDataFrame()

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

