library(shiny)
library(nbastatR)
Sys.setenv("VROOM_CONNECTION_SIZE"=2000000)
HOFlogo = "https://upload.wikimedia.org/wikipedia/fr/3/3d/Basketball_Hall_of_Fame_%28logo%29.png"
getBestScorers <- function(season_range) {
  players_stats <- read.csv("bref_stats.csv")
  season_range <- season_range[1]:season_range[2]
  players_stats <- players_stats[players_stats$yearSeason %in% season_range, ]
  relevant_cols <- c("namePlayer", "minutes", "slugSeason", "slugPosition", "agePlayer", "slugTeamBREF", "countGames", "isHOFPlayer", "slugTeamsBREF", "urlPlayerHeadshot", "ratioPER", "pctFG", "ptsPerGame", "trbPerGame", "astPerGame", "stlPerGame", "blkPerGame")
  players_stats <- players_stats[, relevant_cols]
  return(players_stats)
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

# Function to render scatter plot
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


