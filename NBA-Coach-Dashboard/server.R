#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(nbastatR)

# Define server logic required to draw a histogram
function(input, output, session) {
  output$table <- renderTable({
    # Fetch the data for the selected team and season
    team_data <- teams_rosters(team = input$team, season = input$season)
    
    # Perform the ranking based on the selected option
    if (input$option == "Tall") {
      team_data <- team_data[order(-team_data$height),]
    } else if (input$option == "Best") {
      # For "Best" option, you might want to consider a parameter like player efficiency rating (PER)
      # Let's assume we have a column named 'PER' in the dataset
      team_data <- team_data[order(-team_data$PER),]
    } else if (input$option == "Young") {
      team_data <- team_data[order(team_data$age),]
    }
    
    # Get the top 5 players
    team_data <- head(team_data, 5)
    return(team_data)
  })
}

