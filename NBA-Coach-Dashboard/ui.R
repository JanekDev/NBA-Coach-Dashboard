library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(flexdashboard)



startingFivePage <- function(){
  sidebarLayout(
    sidebarPanel(
      width = 2,
      uiOutput("select_team"),
    ),
    mainPanel(
      fluidRow(
        # column(10, h4("TEXT"), br(), plotOutput("starting_five_plot_PG")),
        # column(10, h4("TEXT"), br(), plotOutput("starting_five_plot_SG")),
        # column(2, h4("TEXT"), br(), plotOutput("starting_five_plot_PF")),
        # column(2, h4("TEXT"), br(), plotOutput("starting_five_plot_SF")),
        # column(2, h4("TEXT"), br(), plotOutput("starting_five_plot_C"))
      )
    )
  )
}

playerMatchupPage <- function(){
  sidebarLayout(
    sidebarPanel(
      width = 2,
      uiOutput("select_stats"),
    ),
    mainPanel(
      fluidRow(
        column(5, uiOutput("select_matchup_left"), br(), plotOutput("player_matchup_left")),
        column(5, uiOutput("select_matchup_right"), br(), plotOutput("player_matchup_right")),
      )
    )
  )
}
draftPicksPage <- function(){}
aboutPage <- function(){}

shinyUI(
  fluidPage(
    navbarPage(
      "NBA Coach Dashboard",
      theme = shinytheme("sandstone"),
      tabPanel("Starting 5", startingFivePage()),
      tabPanel("Player matchup", playerMatchupPage()),
      tabPanel("Draft Picks", draftPicksPage()),
      tabPanel("About", aboutPage()),
    )
  )
)


