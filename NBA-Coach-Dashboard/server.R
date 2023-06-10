library(shiny)
library(nbastatR)

getPlayersStats <- function (){
  players_stats = bref_players_stats(
    tables = c("advanced", "per_game"),
    seasons = 2023,
    assign_to_environment = FALSE
  )
  relevant_cols = c("namePlayer","minutes", "slugPosition", "agePlayer", "slugTeamBREF", "countGames", "isHOFPlayer", "slugTeamsBREF", "urlPlayerHeadshot", "ratioPER", "trbPerGame", "astPerGame", "stlPerGame", "blkPerGame", "ptsPerGame")
  # get only players with minutes > 144
  players_stats = players_stats[players_stats$minutes > 144, relevant_cols]
  return(players_stats)
}

function(input, output, session) {

  players_stats = getPlayersStats()
  
  output$select_team <- renderUI(
    {
      selectInput(
        "select_team",
        "Select Team",
        unique(players_stats$slugTeamBREF)
      )
    }
  )
  
  output$table <- renderTable({
   
    # # Perform the ranking based on the selected option
    # if (input$option == "Tall") {
    #   team_data <- team_data[order(-team_data$height),]
    # } else if (input$option == "Best") {
    #   # For "Best" option, you might want to consider a parameter like player efficiency rating (PER)
    #   # Let's assume we have a column named 'PER' in the dataset
    #   team_data <- team_data[order(-team_data$PER),]
    # } else if (input$option == "Young") {
    #   team_data <- team_data[order(team_data$age),]
    # }
    # 
    # # Get the top 5 players
    # team_data <- head(team_data, 5)
    # return(team_data)
    return(filter(players_stats, slugTeamBREF=="TOT"))
  })
}

