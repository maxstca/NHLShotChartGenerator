#Load packages
library(httr)
library(jsonlite)
library(tidyverse)
library(spacesXYZ)

#RDA files
load("team_logos_colors.rda") #Data sourced from github user danmorse314 via the hockeyR package.

#Edit some values from the above dataframe to avoid scenarios where obs. are plotted in white against a white background
team_logos_colors$team_color2[11] <- "#BDAD7E" #DET alt
team_logos_colors$team_color1[13] <- "#C8102E" #Swap FLA colors
team_logos_colors$team_color2[13] <- "#041E42" #Swap FLA colors
team_logos_colors$team_color2[27] <- "#808080"  #TBL Alt
team_logos_colors$team_color2[28] <- "#000000" #TOR Alt

#Base URL to send to helpers
base <- "https://api-web.nhle.com/v1/gamecenter/"

#Create a color difference matrix for the accessibility check later down the road
deltaE <- matrix(, nrow = nrow(team_logos_colors), ncol = nrow(team_logos_colors))
dimnames(deltaE) <- list(team_logos_colors$team_abbr, team_logos_colors$team_abbr)

getRGBFromHex <- function(hexCode = "#FFFFFF") {
  rgb_array <- col2rgb(hexCode)
  rgb_vec <- c(rgb_array[1], rgb_array[2], rgb_array[3])
  return(rgb_vec)
}

enterMatrixValue <- function(matrix, rowIndex = 1, colIndex = 1) {
  deltaVal <- DeltaE(getRGBFromHex(team_logos_colors$team_color1[rowIndex]), getRGBFromHex(team_logos_colors$team_color1[colIndex]))
  matrix[rowIndex, colIndex] <- deltaVal
  matrix[colIndex, rowIndex] <- deltaVal
  return(matrix)
}

for (i in 1:nrow(deltaE)) {
  for (j in 1:ncol(deltaE)) {
    if (j < i) {
      next
    } else {
      deltaE <- enterMatrixValue(deltaE, i, j)
    }
  }
}

#Manually reduce deltaE for some edge cases that occur on occasion
deltaE["BUF", "STL"] <- deltaE["STL", "BUF"] <- 10

#Grabs basic game information (teams, team colors, etc.)
getGameInfoByDate <- function(input_date = format(Sys.Date(), format = "%Y-%m-%d")) {
  
  string <- paste0("https://api-web.nhle.com/v1/schedule/", input_date)
  returned_raw <- httr::GET(string)
  returned <- fromJSON(rawToChar(returned_raw$content))
  
  games.df <- returned$gameWeek |>
    filter(date == input_date) |>
    select(games)
  
  games.df <- games.df$games
  id <- games.df[[1]]$id
  date <- format(as.Date(input_date, format = "%Y-%m-%d"), format = "%m-%d-%Y")
  homeTeam <- games.df[[1]]$homeTeam
  homeTeamAbbrev <- homeTeam$abbrev
  homeLogo <- homeTeam$logo
  homeID <- homeTeam$id
  awayTeam <- games.df[[1]]$awayTeam
  awayTeamAbbrev <- awayTeam$abbrev
  awayLogo <- awayTeam$logo
  awayID <- awayTeam$id
  
  #TODO: Collect some more info about the game, namely date & time.

  gameInfo <- data.frame(id, date, homeID, homeTeamAbbrev, homeLogo, awayID, awayTeamAbbrev, awayLogo)
  
  return(gameInfo)
}

getTeamLogosColors <- function(homeTeamAbbr = "CAR", awayTeamAbbr = "CAR") {
  logosColors <- c("a", "b", "c", "d")
  logosColors[1] <- team_logos_colors$team_color1[team_logos_colors$team_abbr == homeTeamAbbr]
  
  #Check deltaE values for similar colors, defer to alternate on the away team if this is the case:
  if (deltaE[homeTeamAbbr, awayTeamAbbr] < 50) {
    logosColors[2] <- team_logos_colors$team_color2[team_logos_colors$team_abbr == awayTeamAbbr]
  } else {
    logosColors[2] <- team_logos_colors$team_color1[team_logos_colors$team_abbr == awayTeamAbbr]
  }
  
  logosColors[3] <- team_logos_colors$team_logo_espn[team_logos_colors$team_abbr == homeTeamAbbr]
  logosColors[4] <- team_logos_colors$team_logo_espn[team_logos_colors$team_abbr == awayTeamAbbr]
  
  return(logosColors)
}

getPBPraw <- function(gameID = "2025020001") {
  string <- paste0("https://api-web.nhle.com/v1/gamecenter/", gameID, "/play-by-play")
  returned_raw <- httr::GET(string)
  returned <- fromJSON(rawToChar(returned_raw$content))
  
  return(returned)
}

getShotData <- function(gameID = "2025020001") {
  df <- getPBPraw(gameID)
  tryCatch({
    suppressWarnings(
      returned <- df$plays |>
        unnest() |>
        filter(typeCode %in% (505:508))
    )  
    
    return(returned)
  }, error = function (e) {
    return(NULL)
  }
  )
  
  
}

getPlayerInfo <- function(rawPBP, gameInfo) {
  players <- rawPBP$rosterSpots |>
    select(teamId, playerId, sweaterNumber, positionCode, headshot)
  
  rosterSpots <- rawPBP$rosterSpots
  firstNames <- rosterSpots$firstName
  lastNames <- rosterSpots$lastName
  playerNames <- c(firstNames$default, lastNames$default)
  players <- cbind(players, firstNames$default, lastNames$default)
  colnames(players) <- c("TeamID", "PlayerID", "sweaterNumber", "positionCode", "headshot", "firstName", "lastName")
  
  teams <- gameInfo |>
    filter(id == rawPBP$id)
  teams <- data.frame(c(teams$homeID, teams$awayID),c(teams$homeTeamAbbrev, teams$awayTeamAbbrev))
  colnames(teams) <- c("id", "TeamCode")
  
  colors <- getTeamLogosColors(homeTeamAbbr = teams$TeamCode[1], awayTeamAbbr = teams$TeamCode[2])
  
  #Join team info to each player
  players <- players |>
    left_join(teams, by = join_by(TeamID == id)) |>
    mutate(teamColor = if_else(TeamID == teams$id[1], colors[1], colors[2]),
           homeTeamAbbr = teams$TeamCode[1],
           awayTeamAbbr = teams$TeamCode[2],
           teamLogo = if_else(TeamID == teams$id[1], colors[3], colors[4]))
  
  return(players)
}

