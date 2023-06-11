library(shiny)
library(fmsb)
library(dplyr)
library(magrittr)
library(bslib)
library(DT)

Sys.setenv("VROOM_CONNECTION_SIZE"= 2000000)

AVAILABLE_SEASONS =  c(2023)
HOFlogo = "https://upload.wikimedia.org/wikipedia/fr/3/3d/Basketball_Hall_of_Fame_%28logo%29.png"
RELEVANT_COLS = c("namePlayer","minutes", "slugPosition", "agePlayer", "slugTeamBREF", "countGames", "isHOFPlayer", "slugTeamsBREF", "urlPlayerHeadshot", "ratioPER", "trbPerGame", "astPerGame", "stlPerGame", "blkPerGame", "ptsPerGame")
STATS_COLS = c("trbPerGame",  "astPerGame",  "stlPerGame","blkPerGame", "ptsPerGame")
STATS_FULL_NAMES = c("Rebounds Per Game",  "Assits Per Game",  "Steals Per Game","Blocks Per Game", "Points Per Game")
STATS_SLUGS = c("RPG",  "APG",  "SPG",  "BPG",  "PPG")
names(STATS_COLS) <- STATS_FULL_NAMES
names(STATS_SLUGS) <- STATS_COLS

RATING_COL = c("ratioPER")
names(RATING_COL) <- c("Ratio Per Game")

RATING_COLS = c(STATS_COLS, RATING_COL, "agePlayer")

# getPlayersDataFrame <- function (season = 2023){
#   players_stats = bref_players_stats(
#     tables = c("advanced", "per_game"),
#     seasons = season,
#     assign_to_environment = FALSE
#   )
#   relevant_cols = RELEVANT_COLS
#   return(players_stats[players_stats$minutes > 144,relevant_cols])
# }

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
  if(team == "TOT"){
    team = NULL
  }
  if(!is.null(team)){
    players_df %<>% dplyr::filter(slugTeamsBREF==team)
  }
  if(!is.null(position)){
    players_df %<>% dplyr::filter(slugPosition==position)
  }
  mask = players_df[,stat_col] == getStats(players_df, team =team, position = position, stats_cols = stat_col)
  return(players_df[mask,])
}

getTeamsTotal <- function(PG,SG,PF,SF,C, stats_cols=STATS_COLS){
  (PG[,stats_cols] + SG[,stats_cols] + PF[,stats_cols] + SF[,stats_cols] + C[,stats_cols])/5
}

player_stats_spider_plot <- function(player, best_stats, color=c("blue", "yellow"), stats_cols=STATS_COLS, type=0, mar=rep(0,4)){
  data <- rbind(
    best_stats[stats_cols],
    rep(0,length(stats_cols)),
    player[,stats_cols]
    )
  par(mar = mar)
  radarchart(
    data, axistype = type,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    
    # Customize the axis
    axislabcol = "grey",
    vlabels = STATS_SLUGS[stats_cols]
  )
}

getBestScorers <- function(season_range) {
  players_stats <- read.csv("bref_stats.csv")
  season_range <- season_range[1]:season_range[2]
  players_stats <- players_stats[players_stats$yearSeason %in% season_range, ]
  relevant_cols <- c("namePlayer", "minutes", "slugSeason", "slugPosition", "agePlayer", "slugTeamBREF", "countGames", "isHOFPlayer", "slugTeamsBREF", "urlPlayerHeadshot", "ratioPER", "pctFG", "ptsPerGame", "trbPerGame", "astPerGame", "stlPerGame", "blkPerGame")
  players_stats <- players_stats[, relevant_cols]
  return(players_stats)
}

getPlayersDataFrame <- function (season = 2023){
  tibble(getBestScorers(c(season,season)))[,RELEVANT_COLS]
}

axisLabels <- list(
  ratioPER = "Player Efficiency Rating", 
  pctFG = "Field Goal %", 
  ptsPerGame = "Points per Game", 
  trbPerGame = "Rebounds per Game", 
  astPerGame = "Assists per Game", 
  stlPerGame = "Steals per Game", 
  blkPerGame = "Blocks per Game"
)

renderScatterPlot <- function(best_scorers, selected_player, x_axis, y_axis) {
  renderPlotly({
    plot_data <- best_scorers()
    selected_data <- plot_data[plot_data$namePlayer %in% selected_player(),]
    
    base_plot <- plot_ly(
      data = plot_data,
      x = plot_data[[x_axis()]], y = plot_data[[y_axis()]],  # Change to dynamic input
      type = 'scatter', mode = 'markers',
      hoverinfo = 'text', text = ~paste(" Player: ", namePlayer, "<br>",
                                        "Season: ", slugSeason, "<br>"),
      key = ~namePlayer,
      source = 'scatter',
      marker = list(color = "rgba(0,0,200,0.5)", size = 6)  # base color and size for all points
    )
    
    selected_plot <- add_trace(
      base_plot,
      data = selected_data,
      x = selected_data[[x_axis()]], y = selected_data[[y_axis()]],  # Change to dynamic input
      type = 'scatter', mode = 'markers',
      marker = list(color = "red", size = 12),  # specific color and size for selected points
      hoverinfo = 'none',  # disable hover info for this trace
      showlegend = FALSE,  # hide legend for this trace
      inherit = FALSE  # prevent inheriting attributes from the base plot
    )
    
    x_label <- isolate(axisLabels[[x_axis()]])
    y_label <- isolate(axisLabels[[y_axis()]])
    
    selected_plot %>% layout(
      xaxis = list(title = x_label),  # Use labels from the map
      yaxis = list(title = y_label)  # Use labels from the map
    )
  })
}

renderPlayerInfo <- renderPlayerInfo <- function(best_scorers, selected_player) {
  renderUI({
    selected_data <- best_scorers()[best_scorers()$namePlayer %in% selected_player(),]
    if(nrow(selected_data) > 0){
      playerMugshot <- selected_data$urlPlayerHeadshot[1]
      defaultMugshot <- "def_baller.png"
      
      if (selected_data$isHOFPlayer[1]) {  # If the player is a Hall of Famer
        fluidRow(
          column(1, h4(selected_data$namePlayer[1]), img(src = HOFlogo, height = "50", width = "50")),
          column(2,
                 tags$img(src = playerMugshot, height = "133", width = "182", onerror = paste0("this.onerror=null; this.src='", defaultMugshot, "'; this.height='133'; this.width='133';"))
          )
        )
      } else {  # If the player is not a Hall of Famer
        fluidRow(
          column(1, h4(selected_data$namePlayer[1])),
          column(2, tags$img(src = playerMugshot, height = "133", width = "182", onerror = paste0("this.onerror=null; this.src='", defaultMugshot, "'; this.height='133'; this.width='133';"))
          )
        )
      }
    }
  })
}

# Prepare player data for datatable
preparePlayerData <- function(best_scorers, selected_player) {
  selected_point <- best_scorers()[best_scorers()$namePlayer == selected_player(),]
  
  # Exclude certain columns from the data table
  selected_point <- selected_point[ , -which(names(selected_point) %in% c("namePlayer", "slugTeamBREF", "isHOFPlayer", "urlPlayerHeadshot"))]
  
  # Rename the columns
  names(selected_point) <- c("Minutes", "Season", "Position", "Age", "Games Count", "Teams", "PER Ratio", "FG%", "PTS per Game", "TRB per Game", "AST per Game", "STL per Game", "BLK per Game")
  
  # Sort by season
  selected_point <- selected_point[order(selected_point$Season, decreasing = TRUE), ]
  
  return(selected_point)
}

renderPlayerTable <- function(player_data) {
  DT::renderDataTable({
    DT::datatable(player_data(), options = list(pageLength = 25))
  })
}

function(input, output, session) {
  
  players_df = reactive(getPlayersDataFrame(input$select_season))
  
  best_stats_in_season <- reactive(getStats(players_df()))
  avg_stats_in_season <- reactive(getStats(players_df(), fun = function(x) mean(x)))
  
  #starting_five
  output$select_team <- renderUI({
    selectInput(
      "select_team",
      "Select Team",
      choices = unique(players_df()[,"slugTeamBREF"]),
      #TODO label
      selected = "TOR"
    )
  })

  starting_five_player_PG <- reactive({getBestPlayer(players_df(), position="PG", team=input$select_team) %>% head(n=1)})
  starting_five_player_SG <- reactive({getBestPlayer(players_df(), position="SG", team=input$select_team) %>% head(n=1)})
  starting_five_player_PF <- reactive({getBestPlayer(players_df(), position="PF", team=input$select_team) %>% head(n=1)})
  starting_five_player_SF <- reactive({getBestPlayer(players_df(), position="SF", team=input$select_team) %>% head(n=1)})
  starting_five_player_C <- reactive({getBestPlayer(players_df(), position="C", team=input$select_team) %>% head(n=1)})
  starting_five_team <- reactive({
    getTeamsTotal(
      starting_five_player_PG(),
      starting_five_player_SG(),
      starting_five_player_PF(),
      starting_five_player_SF(),
      starting_five_player_C(),
      stats_cols = RATING_COLS
  )
  })
  
  output$starting_five_player_name_PG <- renderText({paste(starting_five_player_PG()[,"namePlayer"])})
  output$starting_five_player_name_SG <- renderText({paste(starting_five_player_SG()[,"namePlayer"])})
  output$starting_five_player_name_PF <- renderText({paste(starting_five_player_PF()[,"namePlayer"])})
  output$starting_five_player_name_SF <- renderText({paste(starting_five_player_SF()[,"namePlayer"])})
  output$starting_five_player_name_C <- renderText({paste(starting_five_player_C()[,"namePlayer"])})
  
  output$starting_five_player_image_PG <-  renderText({paste0('<img height="80" onerror="this.src=', defaultMugshot, '" src="', starting_five_player_PG()[,"urlPlayerHeadshot"] ,'">')})
  output$starting_five_player_image_SG <-  renderText({paste0('<img height="80" onerror="this.src=', defaultMugshot, '" src="', starting_five_player_SG()[,"urlPlayerHeadshot"] ,'">')})
  output$starting_five_player_image_PF <-  renderText({paste0('<img height="80" onerror="this.src=', defaultMugshot, '" src="', starting_five_player_PF()[,"urlPlayerHeadshot"] ,'">')})
  output$starting_five_player_image_SF <-  renderText({paste0('<img height="80" onerror="this.src=', defaultMugshot, '" src="', starting_five_player_SF()[,"urlPlayerHeadshot"] ,'">')})
  output$starting_five_player_image_C <-  renderText({paste0('<img height="80"  onerror="this.src=', defaultMugshot, '" src="', starting_five_player_C()[,"urlPlayerHeadshot"] ,'">')})
  
  output$starting_five_player_info_PG <- renderUI({
    card(
      card_header(
        class = "btn-primary",
        renderText(paste0("Position: ",starting_five_player_PG()[, c("slugPosition")]),),
      ),
      card_body(
        class = "bg-info",
        renderText(paste0("Age: ", starting_five_player_PG()[, c("agePlayer")])),
        #renderText(paste0("Total number of minutes:", starting_five_player_PG()[, c("minutes")])),
        # renderText(paste0("Games:", starting_five_player_PG()[, c("countGames")]))
        renderText(paste0("PER:", starting_five_player_PG()[, c("ratioPER")])),
        renderText(paste0("Points:", starting_five_player_PG()[, c("ptsPerGame")]))
      )
    )
  })
  output$starting_five_player_info_SG <- renderUI({
    card(
      card_header(
        class = "btn-primary",
        renderText(paste0("Position: ",starting_five_player_SG()[, c("slugPosition")]),),
      ),
      card_body(
        class = "bg-info",
        renderText(paste0("Age: ", starting_five_player_SG()[, c("agePlayer")])),
        # renderText(paste0("Total number of minutes:", starting_five_player_PG()[, c("minutes")])),
        # renderText(paste0("Total number of games:", starting_five_player_PG()[, c("countGames")])),
        renderText(paste0("PER:", starting_five_player_SG()[, c("ratioPER")])),
        renderText(paste0("Points:", starting_five_player_SG()[, c("ptsPerGame")]))
      )
    )
  })
  output$starting_five_player_info_PF <- renderUI({
    card(
      card_header(
        class = "btn-primary",
        renderText(paste0("Position: ",starting_five_player_PF()[, c("slugPosition")]),),
      ),
      card_body(
        class = "bg-info",
        renderText(paste0("Age: ", starting_five_player_PF()[, c("agePlayer")])),
        # renderText(paste0("Total number of minutes:", starting_five_player_PG()[, c("minutes")])),
        # renderText(paste0("Total number of games:", starting_five_player_PG()[, c("countGames")])),
        renderText(paste0("PER:", starting_five_player_PF()[, c("ratioPER")])),
        renderText(paste0("Points:", starting_five_player_PF()[, c("ptsPerGame")]))
      )
    )
  })
  output$starting_five_player_info_SF <- renderUI({
    card(
      card_header(
        class = "btn-primary",
        renderText(paste0("Position: ",starting_five_player_SF()[, c("slugPosition")]),),
      ),
      card_body(
        class = "bg-info",
        renderText(paste0("Age: ", starting_five_player_SF()[, c("agePlayer")])),
        # renderText(paste0("Total number of minutes:", starting_five_player_PG()[, c("minutes")])),
        # renderText(paste0("Total number of games:", starting_five_player_PG()[, c("countGames")])),
        renderText(paste0("PER:", starting_five_player_SF()[, c("ratioPER")])),
        renderText(paste0("Points:", starting_five_player_SF()[, c("ptsPerGame")]))
      )
    )
  })
  output$starting_five_player_info_C <- renderUI({
    card(
      card_header(
        class = "btn-primary",
        renderText(paste0("Position: ",starting_five_player_C()[, c("slugPosition")]),),
      ),
      card_body(
        class = "bg-info",
        renderText(paste0("Age: ", starting_five_player_C()[, c("agePlayer")])),
        # renderText(paste0("Total number of minutes:", starting_five_player_PG()[, c("minutes")])),
        # renderText(paste0("Total number of games:", starting_five_player_PG()[, c("countGames")])),
        renderText(paste0("PER:", starting_five_player_C()[, c("ratioPER")])),
        renderText(paste0("Points:", starting_five_player_C()[, c("ptsPerGame")]))
      )
    )
  })
  output$starting_five_team_info <- renderUI({
    card(
      card_header(
        class = "btn-primary",
        "SQUAD'S AVERGAGE"
      ),
      card_body(
        class = "bg-info",
        renderText(paste0("Age: ", starting_five_team()[, c("agePlayer")])),
        # renderText(paste0("Total number of minutes:", starting_five_player_PG()[, c("minutes")])),
        # renderText(paste0("Games:", starting_five_player_PG()[, c("countGames")])),
        renderText(paste0("PER:", starting_five_team()[, c("ratioPER")])),
        renderText(paste0("Points:", starting_five_team()[, c("ptsPerGame")]))
      )
    )
  })
  
  output$starting_five_player_plot_PG <- renderPlot({player_stats_spider_plot(starting_five_player_PG(), best_stats_in_season())}, height=150)
  output$starting_five_player_plot_SG <- renderPlot({player_stats_spider_plot(starting_five_player_SG(), best_stats_in_season())}, height=150)
  output$starting_five_player_plot_PF <- renderPlot({player_stats_spider_plot(starting_five_player_PF(), best_stats_in_season())}, height=150)
  output$starting_five_player_plot_SF <- renderPlot({player_stats_spider_plot(starting_five_player_SF(), best_stats_in_season())}, height=150)
  output$starting_five_player_plot_C <- renderPlot({player_stats_spider_plot(starting_five_player_C(), best_stats_in_season())}, height=150)
  output$starting_five_player_plot_team <- renderPlot({player_stats_spider_plot(rbind(starting_five_team(), avg_stats_in_season()), best_stats_in_season())}, height=150)
  
  output$starting_five_team_table <- DT::renderDataTable({
    players_df() %>% 
      dplyr::filter(slugTeamsBREF==input$select_team) %>% 
      select(c("namePlayer", "slugPosition", "agePlayer", "minutes", "countGames","ratioPER", "trbPerGame", "astPerGame", "stlPerGame", "blkPerGame", "ptsPerGame"))
  })
  
  #matchup
  output$select_stats <- renderUI({
    selectInput(
      "select_stats",
      "Statistics",
      choices = STATS_COLS,
      selected = STATS_COLS,
      multiple = TRUE
    )
  })
  output$select_matchup_left <- renderUI({
    selectInput(
      "select_matchup_left",
      "Select Left Player For Matchup",
      choices = unique(players_df()[,"namePlayer"]),
      selected = "Stephen Curry"
    )
  })
  output$select_matchup_right <- renderUI({
    selectInput(
      "select_matchup_right",
      "Select Right Player For Matchup",
      choices = unique(players_df()[,"namePlayer"]),
      selected = "LeBron James"
    )
  })
  output$select_matchup_metric <- renderUI({
    selectInput(
      "select_matchup_metric",
      "Matchup Metric",
      choices = c(RATING_COL,STATS_COLS),
      selected = RATING_COL
    )
  })
  
  player_matchup_left <- reactive({players_df() %>% dplyr::filter(namePlayer==input$select_matchup_left)})
  player_matchup_right <- reactive({players_df() %>% dplyr::filter(namePlayer==input$select_matchup_right)})
  score_matchup_left <- reactive({player_matchup_left()[1,input$select_matchup_metric]})
  score_matchup_right <- reactive({player_matchup_right()[1,input$select_matchup_metric]})
  
  output$image_matchup_left <- renderText({paste0('<img src="', player_matchup_left()[1,"urlPlayerHeadshot"] ,'">')})
  output$image_matchup_right <- renderText({paste0('<img src="', player_matchup_right()[1,"urlPlayerHeadshot"] ,'">')})
  
  output$card_score_matchup_left <- renderUI({
    card(
      height = 50,
      card_header(
        class = if (score_matchup_right() < score_matchup_left()) "btn-success" else "btn-danger",
        if (score_matchup_right() < score_matchup_left()) "WINNER" else "LOOSER",
        
      ),
      card_body(
        class = if (score_matchup_right() < score_matchup_left()) "bg-success" else "bg-danger",
        paste("Score:", score_matchup_left(), sep = " ")
      )
    )
    })
  output$card_score_matchup_right <- renderUI({
    card(
      height = 50,
      card_header(
        class = if (score_matchup_right() > score_matchup_left()) "btn-success" else "btn-danger",
        if (score_matchup_right() > score_matchup_left()) "WINNER" else "LOOSER",
        
      ),
      card_body(
        class = if (score_matchup_right() > score_matchup_left()) "bg-success" else "bg-danger",
        paste("Score:", score_matchup_right(), sep = " ")
      )
    )
  })
  
  output$plot_player_matchup_left <- renderPlot({player_stats_spider_plot(player_matchup_left() ,best_stats_in_season(), stats_cols = input$select_stats, type=1)}) 
  output$plot_player_matchup_right <- renderPlot({player_stats_spider_plot(player_matchup_right(), best_stats_in_season(), stats_cols = input$select_stats, type=1)})
  
  selected_player <- reactiveVal()
  
  best_scorers <- reactive({
    data <- getBestScorers(input$season_range)
    updateSelectizeInput(session, 'playerName', choices = unique(data$namePlayer), selected = selected_player())
    data
  })
  
  observeEvent(input$playerName, {
    # Only update selected_player if the input matches a player name
    if (input$playerName %in% unique(best_scorers()$namePlayer)) {
      selected_player(input$playerName)
    }
  }, ignoreNULL = TRUE)
  
  output$scatter_plot <- renderScatterPlot(best_scorers, selected_player, reactive({input$x_axis}), reactive({input$y_axis}))
  
  output$player_info <- renderPlayerInfo(best_scorers, selected_player)
  
  # Prepare player data and then render the table
  player_data <- reactive({ preparePlayerData(best_scorers, selected_player) })
  output$player_table <- renderPlayerTable(player_data)
  
  observeEvent(event_data("plotly_click", source = "scatter"), {
    if (!is.null(event_data("plotly_click", source = "scatter"))) {
      clicked_point <- event_data("plotly_click", source = "scatter")$key
      selected_player(clicked_point)
    }
  })
}
