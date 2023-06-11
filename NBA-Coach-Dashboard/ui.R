library(shiny)
library(shinythemes)
library(bslib)



startingFivePage <- function(){
  fluidPage(
    fluidRow(
      uiOutput("select_team"),
    ),
    fluidRow(
        fluidRow(
        column(2, align="center",
                textOutput("starting_five_player_name_PG"),
                htmlOutput("starting_five_player_image_PG"),
                htmlOutput("starting_five_player_info_PG"),
                plotOutput("starting_five_player_plot_PG")
               ),
        column(2, align="center",
               textOutput("starting_five_player_name_SG"),
               htmlOutput("starting_five_player_image_SG"),
               htmlOutput("starting_five_player_info_SG"),
               plotOutput("starting_five_player_plot_SG")
        ),
        column(2, align="center",
               textOutput("starting_five_player_name_PF"),
               htmlOutput("starting_five_player_image_PF"),
               htmlOutput("starting_five_player_info_PF"),
               plotOutput("starting_five_player_plot_PF")
        ),
        column(2, align="center",
               textOutput("starting_five_player_name_SF"),
               htmlOutput("starting_five_player_image_SF"),
               htmlOutput("starting_five_player_info_SF"),
               plotOutput("starting_five_player_plot_SF")
        ),
        column(2, align="center",
               textOutput("starting_five_player_name_C"),
               htmlOutput("starting_five_player_image_C"),
               htmlOutput("starting_five_player_info_C"),
               plotOutput("starting_five_player_plot_C")
        ),
        column(2, align="center",
               br(),
               h4("TEAMS TOTAL"),
               br(),
               plotOutput("starting_five_player_plot_team")
        )
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
               htmlOutput("card_score_matchup_right"),
               plotOutput("player_matchup_right")
        ),
      )
    )
  )
}
draftPicksPage <- function(){}
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
      tabPanel("Starting 5", startingFivePage()),
      tabPanel("Player matchup", playerMatchupPage()),
      tabPanel("Draft Picks", draftPicksPage()),
      tabPanel("About", aboutPage()),
    )
  )
)


