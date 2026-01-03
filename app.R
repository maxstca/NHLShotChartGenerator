#Packages
library(shiny)
library(bslib)
library(tidyverse)
library(Cairo)
options(shiny.usecairo=T)

#Helper files
source("ShotChartGenerator.R")
source("CollectShotData.R")

# UI

ui <- fluidPage(
  
  # Application title
  titlePanel("NHL Shot Chart Generator"),
  
  sidebarPanel(
              
    textInput("date", "Enter a date (MM-DD-YYYY):", value = format(Sys.Date(), format = "%m-%d-%Y")),
    
    fluidRow(
      column(6, actionButton("previousDay", "Previous Day")),
      column(6, actionButton("nextDay", "Next Day"))
    ),
    
    selectInput("game", "Select a game:", choices = list("Loading..." = 1)),
    
    downloadButton("save", "Save as PNG"),
    
    div("Created by Max Campbell.")
  ),
  
  mainPanel(
    card(plotOutput("chart"))
  )                    
  
)

server <- function(input, output, session) {
  
  game_info <- reactive({
    tryCatch({
      getGameInfoByDate(format(as.Date(input$date, format = "%m-%d-%Y"), format = "%Y-%m-%d"))
    }, error = function(e) {
      data.frame()
    })
  })
  
  plot_input <- function() {
    #Check for valid date input
    shiny::validate(
      need(ncol(game_info()) > 0, "Output failed to generate. This can happen if no games are scheduled on the selected date, or if the selected date is invalid.")
    )
    
    #Generate shot chart
    p <- generateShotCharts(game_info(), index = input$game)
    
    #Validate that shot chart exists
    shiny::validate(
      need(is.null(p) == FALSE, "Output failed to generate. This can happen if the selected game has not started yet. Check back later!")
    )
    
    #Display chart
    p
  }
  
  #Update list of games to select from whenever the selected date is updated in the UI.
  observeEvent(input$date, {
    infoStrings <- c()
    df <- game_info()
    
    infoStrings <- c(infoStrings, paste0(df$awayTeamAbbrev, " @ ", df$homeTeamAbbrev))
    
    if (ncol(df) == 0) {
      choicelist <- c("No games found. Try a different date!")
    } else {
      choicelist <- as.list(seq(1:nrow(df)))
      names(choicelist) <- infoStrings
    }
    
    updateSelectInput(session, "game", choices = choicelist)
  })
  
  #Update the date selection when the Previous Day and Next Day buttons are selected.
  observeEvent(input$previousDay, {
    newDate <- format(as.Date(input$date, format = "%m-%d-%Y") - 1, format = "%m-%d-%Y")
    updateTextInput(session, "date", value = newDate)
  })
  
  observeEvent(input$nextDay, {
    newDate <- format(as.Date(input$date, format = "%m-%d-%Y") + 1, format = "%m-%d-%Y")
    updateTextInput(session, "date", value = newDate)
  })
  
  output$chart <- renderPlot({
    plot_input()
  },
  res = 96)
  
  output$save <- downloadHandler(
    filename = "ShotPlot.png",
    content = function(file) {
      ggsave(file,
        plot = plot_input(),
        width = 10,
        height = 6,
        dpi = 300,
        units = "in",
        bg = "white"
        )
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)