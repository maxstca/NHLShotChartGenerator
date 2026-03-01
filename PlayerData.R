library(httr)
library(jsonlite)
library(tidyverse)

source("CollectShotData.R")

gameIDList <- 2024020001:2024021353

for (i in 1:length(gameIDList)) {
  current <- getShotData(gameIDList[i])
  
  if (i == 1) {
    df <- current
  } else {
    df <- rbind(df, current)
  }
  
}