library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(flexdashboard)

axisLabels <- list(
  ratioPER = "PER Ratio", 
  pctFG = "FG%", 
  ptsPerGame = "PTS per Game", 
  trbPerGame = "TRB per Game", 
  astPerGame = "AST per Game", 
  stlPerGame = "STL per Game", 
  blkPerGame = "BLK per Game"
)

startingFivePage <- function(){
  sidebarLayout(
    sidebarPanel(
      uiOutput("select_team"),
      actionButton("update", "Change"),
      hr()
    ),
    mainPanel(
      tableOutput("table")
    )
  )
}

playerMatchupPage <- function(){}
nbaHistoryPage <- function() {
  fluidPage(
    fluidRow(
      column(12,
             sliderInput("season_range", "Season Range:", min = 1950, max = 2023, value = c(2015, 2023), width="100%", sep="")
      )
    ),
    fluidRow(
      column(6,
             selectizeInput('playerName', 'Search Player:', choices = NULL, options = list(maxOptions = 10))
      ),
      column(3,
             selectInput("x_axis", "Select X-axis:", choices = c("Field Goal Percentage" = "pctFG", "Points per Game" = "ptsPerGame", "Rebounds per Game" = "trbPerGame", "Assists per Game" = "astPerGame", "Steals per Game" = "stlPerGame", "Blocks per Game" = "blkPerGame", "Player Efficiency Ratio" = "ratioPER"), selected = "pctFG")
      ),
      column(3,
             selectInput("y_axis", "Select Y-axis:", choices = c("Field Goal Percentage" = "pctFG", "Points per Game" = "ptsPerGame", "Rebounds per Game" = "trbPerGame", "Assists per Game" = "astPerGame", "Steals per Game" = "stlPerGame", "Blocks per Game" = "blkPerGame", "Player Efficiency Ratio" = "ratioPER"), selected = "ptsPerGame")
      )
    ),
    fluidRow(
      column(12,
             plotlyOutput("scatter_plot")
      )
    ),
    fluidRow(
      column(12,
             uiOutput("player_info")
      )
    ),
    fluidRow(
      column(12,
             DT::dataTableOutput("player_table")
      )
    )
  )
}

aboutPage <- function(){}

shinyUI(
  fluidPage(
    navbarPage(
      "NBA Coach Dashboard",
      theme = shinytheme("sandstone"),
      tabPanel("Starting 5", startingFivePage()),
      tabPanel("Player matchup", playerMatchupPage()),
      tabPanel("Statistical NBA History", nbaHistoryPage()),
      tabPanel("About", aboutPage())
    )
  )
)
