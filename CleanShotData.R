library(tidyverse)

## Shot data helper function
#
# This function takes raw PBP data from the NHL API
# and cleans it into a dataset usable for display and visualization.
#
# Parameters:
# game - A dataframe containing information related to a single NHL game.
# includeNonSOG - include blocked and missed shot attempts, default = FALSE.
#
# Returns a dataframe of cleaned shot data.
getCleanedShotData <- function(game, includeNonSOG = FALSE) {
  
  #Get raw info
  raw_pbp <- getPBPraw(game$id)
  
  return_early_flag <- FALSE
  
  #Check for null PBP data and return early if no PBP exists yet.
  tryCatch({
    pbp <<- getShotData(game$id) |> 
      rename(x.Coord = xCoord, y.Coord = yCoord)
    
    if ("scoringPlayerId" %in% colnames(pbp)) {
      pbp <<- pbp |>
        mutate(shootingPlayerId = if_else(typeDescKey == "goal", scoringPlayerId, shootingPlayerId))
    }
    
    player_info <<- getPlayerInfo(raw_pbp, game)
  }, error = function(e) {
    return_early_flag <<- TRUE
  })
  
  if(return_early_flag) {
    return(NULL)
  }
  
  #Filter down to desired event types
  if (!includeNonSOG) {
    sog_types <- c("shot-on-goal", "goal")
  } else {
    sog_types <- c("shot-on-goal", "goal", "missed-shot", "blocked-shot")
  }
  
  #Add player info
  shotdata <- pbp |>
    left_join(player_info, by = join_by(shootingPlayerId == PlayerID)) |>
    filter(typeDescKey %in% sog_types)
  
  #Transform coordinate plane such that visiting team shoots left, home team shoots right
  for (j in 1:nrow(shotdata)) {
    if (shotdata$homeTeamDefendingSide[j] == "right" && shotdata$awayTeamAbbr[j] == shotdata$TeamCode[j]) {
      shotdata$x.Coord[j] <- shotdata$x.Coord[j] * -1
      shotdata$y.Coord[j] <- shotdata$y.Coord[j] * -1
    } else if (shotdata$homeTeamDefendingSide[j] == "right" && shotdata$homeTeamAbbr[j] == shotdata$TeamCode[j]) {
      shotdata$x.Coord[j] <- shotdata$x.Coord[j] * -1
      shotdata$y.Coord[j] <- shotdata$y.Coord[j] * -1
    }
  }
  
  #Remove shootout attempts/goals from the data
  shotdata <- shotdata |>
    filter(periodType != "SO")
  
  shotdata <- shotdata |>
    rowwise() |>
    mutate(shotDistance = ifelse(TeamCode == awayTeamAbbr, getEuclideanDistance(c(x.Coord, y.Coord), c(-89, 0)), getEuclideanDistance(c(x.Coord, y.Coord), c(89, 0))),
           shotAngle = ifelse(TeamCode == awayTeamAbbr, getShotAngle(c(x.Coord, y.Coord), c(-89, 0)), getShotAngle(c(x.Coord, y.Coord), c(89, 0)))) |>
    ungroup()
  
  return(shotdata)
  
}

## Euclidean Distance helper function
#
# This function takes two (x, y) coordinate points, and
# calculates the Euclidean distance between the two.
#
# Parameters:
# point_1 - a vector of length 2. Default c(0, 0).
# point_2 - a vector of length 2. Default c(0, 0).
# 
# Returns a single number representing the Euclidean distance.
getEuclideanDistance <- function(point_1 = c(0, 0), point_2 = c(0, 0)) {
  
  return( sqrt( (point_2[1] - point_1[1])^2 + (point_2[2] - point_1[2])^2 ) )
  
}

## Shot angle helper function
#
# This function takes two (x, y) coordinate points, and
# represents the severity of the angle between the shot location
# and the goal line.
#
# Parameters:
# shot_loc - a vector of length 2 representing the (x, y) location of the shot.
# goal_line - a vector of length 2 representing the goal line. Default c(89, 0).
# 
# Returns a single number representing the shot's angle, in degrees.
getShotAngle <- function(shot_loc, goal_line = c(89, 0)) {
  
  hypoteneuse <- getEuclideanDistance(shot_loc, goal_line)
  opposite <- getEuclideanDistance(shot_loc, c(shot_loc[1], 0))
  
  angle <- asin(opposite / hypoteneuse) * (180 / pi)
  
  
  if (abs(shot_loc[1]) > abs(goal_line[1])) {
    angle <- (90 - angle) + 90
  }
  
  return(angle)
  
}
  