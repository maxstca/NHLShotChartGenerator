library(ggplot2)
library(ggforce)
library(devtools)
library(stringr)
library(glue)
library(magick)
library(ggiraph)

source("CollectShotData.R")
devtools::source_url("https://raw.githubusercontent.com/mrbilltran/the-win-column/master/nhl_rink_plot.R")

generateShotCharts <- function(games, index = 1, includeNonSOG = FALSE) {
  
  #Get base game info
  game <- games[index,]
      
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
  
  #Compute team goals and fetch images for logo overlay
  awaygoals <- nrow(shotdata |>
                      filter(TeamCode == awayTeamAbbr, typeDescKey == "goal"))
  homegoals <- nrow(shotdata |>
                      filter(TeamCode == homeTeamAbbr, typeDescKey == "goal"))
  
  away_img <- magick::image_read(distinct(shotdata |>
                                            filter(TeamCode == awayTeamAbbr),
                                          teamLogo)$teamLogo[1]) |>
    image_colorize(opacity = 60, color = "white")
  
  away_img <- grid::rasterGrob(away_img, interpolate = TRUE)
  
  home_img <- magick::image_read(distinct(shotdata |>
                                            filter(TeamCode == homeTeamAbbr),
                                          teamLogo)$teamLogo[1]) |>
    image_colorize(opacity = 60, color = "white")
  
  home_img <- grid::rasterGrob(home_img, interpolate = TRUE)
  
  #Build strings to use as labels
  scoreline <- glue("{shotdata$awayTeamAbbr[1]} {awaygoals} - {shotdata$homeTeamAbbr[1]} {homegoals}")
  
  if (any(str_detect(shotdata$periodType, "SO"))) {
    #edit scores to match scoring conventions for SO games
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
    
    scoreline <- glue("{shotdata$awayTeamAbbr[1]} {awaygoals} - {shotdata$homeTeamAbbr[1]} {homegoals}")
    scoreline <- paste0(scoreline, " (SO)")
    
    #Remove shootout attempts/goals from the data
    shotdata <- shotdata |>
      filter(periodType != "SO")
    
  } else if (any(str_detect(shotdata$periodType, "OT"))) {
    scoreline <- paste0(scoreline, " (OT)")
  }
  
  #Detect if the game played was a playoff game and update the plot title to reflect the round and game #
  title_string <- paste0("Game ", as.numeric(substr(game$id, 7, 10)))
  
  if (as.numeric(substr(game$id, 5, 6)) == 3) {
    
    if (as.numeric(substr(game$id, 8, 8)) == 4) {
      title_string <- paste0("Stanley Cup Final, Game ", as.numeric(substr(game$id, 10, 10)))
    } else {
      title_string <- paste0("Round ", as.numeric(substr(game$id, 8, 8)), ", Game ", as.numeric(substr(game$id, 10, 10)))
    }

  }
  
  #Count SOG
  awaySOG <- nrow(shotdata |> filter(TeamCode == awayTeamAbbr))
  homeSOG <- nrow(shotdata |> filter(TeamCode == homeTeamAbbr))
  
  #Generate final plot
  chart <- nhl_rink_plot() + 
    annotation_custom(away_img, xmin = -25, xmax = -5, ymin = -10, ymax = 10) +
    annotation_custom(home_img, xmin = 5, xmax = 25, ymin = -10, ymax = 10) +
    geom_point(data = shotdata, aes(x = x.Coord, y = y.Coord, color = teamColor, shape = factor(typeDescKey)), size = 3) +
    scale_color_identity() +
    labs(
      title = glue("{title_string}, {game$date}"),
      subtitle = scoreline,
      shape = "Shot Type",
      caption = glue("Total SOG: {shotdata$awayTeamAbbr[1]} {awaySOG} - {shotdata$homeTeamAbbr[1]} {homeSOG}")
    ) +
    scale_shape_manual(values = c("goal" = 16, "shot-on-goal" = 3), labels = c("Goal", "Shot on Goal")) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.position = "bottom",
          plot.caption = element_text(hjust = 0.5))
  
  return(chart)
}

#examplelist <- generateShotCharts(date = as.Date("12-27-2025", format = "%m-%d-%Y"))