# ExpectedGoals.R
#
# Purpose: derive a basic expected goals model built off data from the 2025-2026 NHL Regular Season

library(tidyverse)
source("CleanShotData.R")

## Shot data helper function
#
# This function takes raw PBP data from the NHL API
# and cleans it into a dataset usable for modeling/visualization.
# Dataset encompasses every regular season game in a given season.
#
# Parameters:
# season - a string with the relevant season code, going by the starting year of the season. Example: 2025-2026 would be '2025'.
# teamCount - the number of teams that played in a given season, default = 32.
# gamesPerTeam - an integer specifying the number of games each team played in a given season, default = 84.
# includeNonSOG - include blocked and missed shot attempts, default = FALSE.
#
# Returns a dataframe of cleaned shot data.
getDataBySeason <- function(season, teamCount = 32, gamesPerTeam = 84, includeNonSOG = FALSE) {
  
  game_ids <- paste0(season, '02', sprintf('%04d', seq(from = 1, to = (teamCount * gamesPerTeam) / 2, by = 1)))
  
  pbplist <- list()
  
  for (game in game_ids) {
    pbplist[[game]] <- getCleanedShotData(game, includeNonSOG)
  }
  
  return (bind_rows(pbplist, .id = 'game_ID'))
  
}


# Use the function calls below to call the data you want to catch, and save it to the cached data file. 
shootingData <- getDataBySeason('2025', teamCount = 32, gamesPerTeam = 82, includeNonSOG = FALSE) 
shootingData <- shootingData |>
  select(game_ID, number, periodType, timeRemaining, situationCode, typeDescKey, x.Coord, y.Coord,
         shotType, shotDistance, shotAngle, awayScore, homeScore, awaySOG, homeSOG, shootingPlayerId, goalieInNetId, firstName, lastName, TeamCode)
save(shootingData, file = "cache/20252026season.RData")