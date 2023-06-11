library(shiny)
library(shinythemes)
library(bslib)
library(DT)
library(plotly)

startingFivePage <- function(){
  fluidPage(
    fluidRow(align = "center",
             h4("Best starting five squad based on Player Efficiency Rating (PER) metric")
    ),
    fluidRow(align = "center",
          column(4,
                  uiOutput("select_team"),
          ),
          column(8,
                 sliderInput("select_season", "Pick Season:", min = 1952, max = 2023, value = c(2023), width="100%", sep="")
          )
    ),
    fluidRow(
        fluidRow(
        column(2, align="center",
                textOutput("starting_five_player_name_PG"),
                htmlOutput("starting_five_player_image_PG"),
                htmlOutput("starting_five_player_info_PG"),
                plotOutput("starting_five_player_plot_PG",height = "100%")
               ),
        column(2, align="center",
               textOutput("starting_five_player_name_SG"),
               htmlOutput("starting_five_player_image_SG"),
               htmlOutput("starting_five_player_info_SG"),
               plotOutput("starting_five_player_plot_SG",height = "100%")
        ),
        column(2, align="center",
               textOutput("starting_five_player_name_PF"),
               htmlOutput("starting_five_player_image_PF"),
               htmlOutput("starting_five_player_info_PF"),
               plotOutput("starting_five_player_plot_PF",height = "100%")
        ),
        column(2, align="center",
               textOutput("starting_five_player_name_SF"),
               htmlOutput("starting_five_player_image_SF"),
               htmlOutput("starting_five_player_info_SF"),
               plotOutput("starting_five_player_plot_SF",height = "100%")
        ),
        column(2, align="center",
               textOutput("starting_five_player_name_C"),
               htmlOutput("starting_five_player_image_C"),
               htmlOutput("starting_five_player_info_C"),
               plotOutput("starting_five_player_plot_C",height = "100%")
        ),
        column(2, align="center",
               htmlOutput("starting_five_team_info"),
               plotOutput("starting_five_player_plot_team", height = "100%")
        )
      ),
      fluidRow(align = "center",
               h3("All players in the team")
      ),
      fluidRow(align="center",
               DT::dataTableOutput("starting_five_team_table")
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
               plotOutput("plot_player_matchup_left")
        ),
        column(6, align="center",
               uiOutput("select_matchup_right"),
               htmlOutput("image_matchup_right"),
               htmlOutput("card_score_matchup_right"),
               plotOutput("plot_player_matchup_right")
        ),
      )
    )
  )
}

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

aboutPage <- function(){
  fluidPage(
    img(src="PP_logotyp_ANG_CMYK.svg", align = "center"),
    h3("This project was created as an assignment for Data visualisation course at Poznan Univeristy of Technology, under the supervision of Ph. D. Dariusz Brzeziński."),
    h4("Source code of this project is publicly available on GitHub under MIT License:"),
    markdown("https://github.com/JanekDev/NBA-Coach-Dashboard"),
    markdown("Created by [Michał Wiliński](https://github.com/JanekDev) & [Łukasz Sztukiewicz](https://github.com/LukaszSztukiewicz)"),
  )
}

shinyUI(
  fluidPage(
    navbarPage(
      "NBA Coach Dashboard",
      theme = shinytheme("sandstone"),
      tabPanel("Starting Five", startingFivePage()),
      tabPanel("Player matchup", playerMatchupPage()),
      tabPanel("Statistical NBA History", nbaHistoryPage()),
      tabPanel("About", aboutPage()),
    )
  )
)
