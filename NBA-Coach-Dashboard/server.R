library(shiny)
library(fmsb)
library(nbastatR)
library(dplyr)
library(magrittr)

AVAILABLE_SEASONS =  c(2023)

STATS_COLS = c( "trbPerGame",  "astPerGame",  "stlPerGame","blkPerGame", "ptsPerGame")
STATS_FULL_NAMES = c("Rebounds Per Game",  "Assits Per Game",  "Steals Per Game","Blocks Per Game", "Points Per Game")
STATS_SLUGS = c("RPG",  "APG",  "SPG",  "BPG",  "PPG")
names(STATS_COLS) <- STATS_FULL_NAMES
names(STATS_SLUGS) <- STATS_COLS

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
  
getStats <- function(players_df, team=NULL, position=NULL, stats_cols = STATS_COLS, fun = function(x) max(x, na.rm = TRUE)){
  if(!is.null(position)){
    players_df %<>% dplyr::filter(slugPosition==position)
  }
  if(!is.null(team)){
    players_df %<>% dplyr::filter(slugTeamsBREF==team)
  }
  return(apply(players_df[,stats_cols], 2, FUN = fun))
}

getBestPlayer <- function(players_df, team=NULL, position=NULL, stat_col = RATING_COL){
  if(!is.null(team)){
    players_df %<>% dplyr::filter(slugTeamsBREF==team)
  }
  if(!is.null(position)){
    players_df %<>% dplyr::filter(slugPosition==position)
  }
  mask = players_df[,stat_col] == getStats(players_df, team =team, position = position, stats_cols = stat_col)
  return(players_df[mask,])
}

player_stats_spider_plot <- function(player, color="blue", stats_cols=STATS_COLS){
  data <- rbind(
    getStats(players_df, stats_cols = stats_cols), #get best statistics in season column wise
    rep(0,length(stats_cols)),
    player[,stats_cols]
    )

  radarchart(
    data, axistype =2,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey",
    vlabels = STATS_SLUGS[stats_cols]
  )
}

getStartingFive <- function(players_df, team){
  players_df %<>% dplyr::filter(slugTeamsBREF==team)
  PG <- head(getBestPlayer(players_df, position="PG"), n=1)
  SG <- head(getBestPlayer(players_df, position="SG"), n=1)
  return(c("PG"=PG, "SG"=SG)) 
}


function(input, output, session) {
  
  players_df = getPlayersDataFrame()

  #starting_five
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
        choices = unique(players_df$slugTeamBREF),
        #TODO label
      )
    })
  output$select_stats <- renderUI({
    selectInput(
      "select_stats",
      "Select Statistics",
      choices = STATS_COLS,
      multiple = TRUE
    )
  })
  output$select_positions <- renderUI({
    selectInput(
      "select_positions",
      "Select Positions",
      choices = unique(players_df$slugPosition),
      #TODO label
      multiple = TRUE
    )
  })
  
  output$table <- renderTable({
    players_df <- filter(players_df, slugTeamBREF==input$select_team)
    return(players_df[,input$select_stats])
  })
  
  starting_five <- reactive({getStartingFive(team=input$select_team)})

  # output$starting_five_plot_PG <- renderPlot({ starting_five()["PG"] %>%  player_stats_spider_plot(stats_cols = input$select_stats)})
  # output$starting_five_plot_SG <- renderPlot({ starting_five("SG") %>%  player_stats_spider_plot(stats_cols = input$select_stats)})
  # output$starting_five_plot_PF <- renderPlot({ getBestPlayer(players_df, position="PF", team=input$select_team) %>% head(n=1) %>%  player_stats_spider_plot(stats_cols = input$select_stats)},height = 400, width = 600)
  # output$starting_five_plot_SF <- renderPlot({ getBestPlayer(players_df, position="SF", team=input$select_team) %>% head(n=1) %>%  player_stats_spider_plot(stats_cols = input$select_stats)},height = 400, width = 600)
  # output$starting_five_plot_C <- renderPlot({ getBestPlayer(players_df, position="C", team=input$select_team) %>% head(n=1) %>% player_stats_spider_plot(stats_cols = input$select_stats)})
  # 
  #matchup
  output$select_matchup_left <- renderUI({
    selectInput(
      "select_matchup_left",
      "Select Left Player For Matchup",
      choices = unique(players_df$namePlayer),
    )
  })
  output$select_matchup_right <- renderUI({
    selectInput(
      "select_matchup_right",
      "Select Right Player For Matchup",
      choices = unique(players_df$namePlayer),
    )
  })
  
  output$player_matchup_left <- renderPlot({ filter(players_df, namePlayer==input$select_matchup_left) %>% player_stats_spider_plot(stats_cols = input$select_stats)}) 
  output$player_matchup_right <- renderPlot({ filter(players_df, namePlayer==input$select_matchup_right) %>% player_stats_spider_plot(stats_cols = input$select_stats)})
}

