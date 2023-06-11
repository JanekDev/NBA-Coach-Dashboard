library(shiny)
library(shinythemes)
library(bslib)



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
      width = 4,
      uiOutput("select_matchup_metric"),
      uiOutput("select_stats")
    ),
    mainPanel(
      fluidRow(
        column(6, align="center",
               uiOutput("select_matchup_left"),
               htmlOutput("image_matchup_left"),
               htmlOutput("card_score_matchup_left"),
               plotOutput("player_matchup_left")
        ),
        column(6, align="center",
               uiOutput("select_matchup_right"),
               htmlOutput("image_matchup_right"),
               htmlOutput("card_score_matchup_left"),
               plotOutput("player_matchup_right")),
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


