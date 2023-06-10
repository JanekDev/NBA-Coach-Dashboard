library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(flexdashboard)



startingFivePage <- function(){
  sidebarLayout(
    sidebarPanel(
     
      uiOutput("select_team"),
    ),
    mainPanel(
      tableOutput("table")
    )
  )
}

playerMatchupPage <- function(){}
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
      uiOutput("select_season")
    )
  )
)


