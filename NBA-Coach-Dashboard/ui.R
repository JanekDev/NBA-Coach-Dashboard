#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

teams = nba_teams_seasons()

fluidPage(
  titlePanel("NBA Starting 5 Advisor"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Choose a Team:", choices = unique(nba_teams$nickname)),
      selectInput("season", "Choose a Season:", choices = unique(seasons$season)),
      selectInput("option", "Choose an Option:", choices = c("Tall", "Best", "Young"))
    ),
    mainPanel(
      tableOutput("table")
    )
  )
)

