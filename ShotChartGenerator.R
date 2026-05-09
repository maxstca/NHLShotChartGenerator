library(ggplot2)
library(ggforce)
library(devtools)
library(stringr)
library(glue)
library(magick)
library(ggiraph)

source("CollectShotData.R")
source("CleanShotData.R")
devtools::source_url("https://raw.githubusercontent.com/mrbilltran/the-win-column/master/nhl_rink_plot.R")

generateShotCharts <- function(games, index = 1, includeNonSOG = FALSE) {
  
  #Get base game info
  game <- games[index,]
  
  shotdata <- getCleanedShotData(game)
  
  if(is.null(shotdata)) { #Error handled by app.R
    return(NULL)
  }

  #Build legend based on game parameters
  legend_args <- buildLegendParams(shotdata)
  
  #Generate final plot
  chart <- nhl_rink_plot() + 
    annotation_custom(getTeamLogoImage(shotdata, shotdata$awayTeamAbbr[1]), xmin = -25, xmax = -5, ymin = -10, ymax = 10) +
    annotation_custom(getTeamLogoImage(shotdata, shotdata$homeTeamAbbr[1]), xmin = 5, xmax = 25, ymin = -10, ymax = 10) +
    geom_point(data = shotdata, aes(x = x.Coord, y = y.Coord, color = teamColor, shape = factor(typeDescKey)), size = 3) +
    scale_color_identity() +
    labs(
      title = buildTitleString(game$id, game$date, game$gameState),
      subtitle = buildScorelineString(shotdata),
      shape = "Shot Type",
      caption = buildCaptionString(shotdata)
    ) +
    scale_shape_manual(values = legend_args[[1]], labels = legend_args[[2]]) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.position = "bottom",
          plot.caption = element_text(hjust = 0.5))
  
  return(chart)
}

## Title string helper function
# Generates a default title based on the last 4 digits of the game's ID.
# From there, this function checks if the game is a playoff game and builds a
# custom title string if true.
#
# Parameters:
# game_id - a 10-digit game-id string provided by NHL API. ex. 2025020001.
# game_date - a string representation of the date that this game is occurring, provided by NHL API.
# game_current_status - a string representation of the current game state, as provided by NHL API. ex. "LIVE" or "CRIT".
#
# Returns a string to use as the plot title.
#
buildTitleString <- function(game_id, game_date, game_current_status) {
  
  # Filter down to last four digits of game ID.
  # Examples: "Game 1" or "Game 1024".
  title_string <- paste0("Game ", as.numeric(substr(game_id, 7, 10)))
  
  #Check if the game type (digits 5 and 6 of game ID) is playoffs (type == 3).
  if (as.numeric(substr(game_id, 5, 6)) == 3) {
    
    #Get the round number (ranges 1-4, defined by 8th digit of game ID)
    if (as.numeric(substr(game_id, 8, 8)) == 4) {
      
      #This is a Stanley Cup Final game, use custom title.
      title_string <- paste0("Stanley Cup Final, Game ", as.numeric(substr(game_id, 10, 10)))
      
    } else {
      
      #This is rounds 1-3, use generic "Round #, Game #" title.
      title_string <- paste0("Round ", as.numeric(substr(game_id, 8, 8)), ", Game ", as.numeric(substr(game_id, 10, 10)))
      
    }
    
  }
  
  #Add the date of the game
  title_string <- paste0(title_string, ", ", game_date)
  
  #If the game is currently ongoing, add a tag to the title to reflect this status
  if(game_current_status %in% c("LIVE", "CRIT")) {
    title_string <- paste0(title_string, " (Live)")
  }
  
  return(title_string)
}

## Scoreline string helper function
# This function takes in a number of goals for each team as well as the overall shot data.
# Then, it checks for some edge cases such as shootout goals, and whether the game went to
# OT or a shootout.
#
# Parameters:
# shotdata - shot data provided by the NHL API and transformed to add team and player info.
#
# Returns a string representing the scoreline of the game. ex. "TOR 1 - NYR 2 (SO)".
#
buildScorelineString <- function(shotdata) {
  
  #Calculate the number of goals that were scored.
  awaygoals <- nrow(shotdata |>
                      filter(TeamCode == awayTeamAbbr, typeDescKey == "goal"))
  homegoals <- nrow(shotdata |>
                      filter(TeamCode == homeTeamAbbr, typeDescKey == "goal"))
  
  #Build a basic scoreline
  scoreline <- glue("{shotdata$awayTeamAbbr[1]} {awaygoals} - {shotdata$homeTeamAbbr[1]} {homegoals}")
  
  #Check if the game went to a shootout.
  if (any(str_detect(shotdata$periodType, "SO"))) {
    
    #Edit scores to match NHL scoring conventions for SO games
    if (homegoals > awaygoals) {
      awaygoals <- nrow(shotdata |>
                          filter(TeamCode == awayTeamAbbr, typeDescKey == "goal", periodType != "SO"))
      homegoals <- nrow(shotdata |>
                          filter(TeamCode == homeTeamAbbr, typeDescKey == "goal", periodType != "SO"))
      homegoals <- awaygoals + 1
    } else {
      awaygoals <- nrow(shotdata |>
                          filter(TeamCode == awayTeamAbbr, typeDescKey == "goal", periodType != "SO"))
      homegoals <- nrow(shotdata |>
                          filter(TeamCode == homeTeamAbbr, typeDescKey == "goal", periodType != "SO"))
      awaygoals <- homegoals + 1
    }
    
    #Add shootout tag to scoreline.
    scoreline <- paste0(scoreline, " (SO)")
    
  } else if (any(str_detect(shotdata$periodType, "OT"))) {
    
    #Add an overtime tag if the game went to overtime.
    scoreline <- paste0(scoreline, " (OT)")
    
  }
  
  return(scoreline)
}

## Caption string helper
#
# This function counts the total shots on goal (SOG) for each team,
# then builds a caption which includes that information.
#
# Parameters:
# shotdata - shot data provided by the NHL API and transformed to add team and player info.
#
# Returns a string representing each team's total SOG (goals + shots on goal).
#
buildCaptionString <- function(shotdata) {
  #Calculate team total shots on goal (SOG)
  awaySOG <- nrow(shotdata |> filter(TeamCode == awayTeamAbbr))
  homeSOG <- nrow(shotdata |> filter(TeamCode == homeTeamAbbr))
  
  #Build SOG string to use as a caption
  caption_string <- glue("Total SOG: {shotdata$awayTeamAbbr[1]} {awaySOG} - {shotdata$homeTeamAbbr[1]} {homeSOG}")
  
  return(caption_string)
}

## Legend helper function
#
# Builds legend based on how many different event types exist at the time of plot generation.
# 
# Parameters:
# shotdata - shot data provided by the NHL API and transformed to add team and player info.
#
# Returns a list with two elements, representing the values argument and labels argument of
# ggplot's `scale_shape_manual()` function.
#
buildLegendParams <- function(shotdata) {
  
  #Calculate the number of goals that were scored.
  awaygoals <- nrow(shotdata |>
                      filter(TeamCode == awayTeamAbbr, typeDescKey == "goal"))
  homegoals <- nrow(shotdata |>
                      filter(TeamCode == homeTeamAbbr, typeDescKey == "goal"))
  
  #Determine how legend should be built based on if a goal has been scored yet
  if (homegoals == 0 && awaygoals == 0) {
    legend_values_arg <- c("shot-on-goal" = 3)
    legend_labels_arg <- c("Shot on Goal")
  } else {
    legend_values_arg <- c("goal" = 16, "shot-on-goal" = 3)
    legend_labels_arg <- c("Goal", "Shot on Goal")
  }
  
  return(legend_args = list(legend_values_arg, legend_labels_arg))
  
}
## Team logo helper function
#
# This function gets a teams logo as provided by the shot data, and
# converts it to a format that ggplot likes.
#
# Parameters:
# shotdata - shot data provided by the NHL API and transformed to add team and player info.
# teamAbbr - the abbreviated code for the desired team logo. Ex. CAR for Carolina Hurricanes.
#
# Returns a raster object compatible with ggplot's `annotation_custom()` function.
getTeamLogoImage <- function(shotdata, teamAbbr) {
  
  current_img <- magick::image_read(distinct(shotdata |>
                                filter(TeamCode == teamAbbr),
                              teamLogo)$teamLogo[1]) |>
    image_colorize(opacity = 60, color = "white")
  
  current_img <- grid::rasterGrob(current_img, interpolate = TRUE)
  
  return(current_img)
}




#examplelist <- generateShotCharts(date = as.Date("12-27-2025", format = "%m-%d-%Y"))