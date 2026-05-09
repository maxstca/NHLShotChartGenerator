#Packages
library(shiny)
library(bslib)
library(tidyverse)
library("cookies")
library(Cairo)
options(shiny.usecairo=T)

#Helper files
source("ShotChartGenerator.R")
source("CollectShotData.R")
source("CleanShotData.R")
source("TimeZoneHelper.R")

# UI

ui <- add_cookie_handlers(
  fluidPage(
    
    # Application title
    titlePanel("NHL Shot Chart Generator"),
    
    navset_pill(
      
      nav_panel("Shot Charts",
                sidebarPanel(
                  
                  dateInput("date", "Enter a date (MM-DD-YYYY):", value = getDate(), format = "mm-dd-yyyy"),
                  
                  fluidRow(
                    column(6, actionButton("previousDay", "Previous Day")),
                    column(6, actionButton("nextDay", "Next Day"))
                  ),
                  
                  selectInput("game", "Select a game:", choices = list("Loading..." = 1)),
                  
                  downloadButton("save", "Save as PNG"),
                ),
                
                mainPanel(
                  card(plotOutput("chart"))
                )              
      ),
      
      nav_panel("Shot Data",
                sidebarPanel(
                  div("Select a different shot chart to change the current game output."),
                  
                  br(),
                  
                  div("Please be aware that this page may return errors at the start of or prior to games as shooting data is typically incomplete at this point."),
                  
                  br(),
                  
                  downloadButton("exportCSV", "Export CSV")
                ),
                
                mainPanel(
                  tableOutput("table")
                )
      ),
      
      nav_panel("Settings",
                sidebarPanel(
                  selectInput("timezone", "Select default timezone for application startup:", choices = timeZones(), 
                              selected = timeZones()[grepl("America/New_York", timeZones())]),
                  
                  actionButton("save_settings", "Save Changes"),
                  
                  div("Note: browser cookies need to be enabled for this website for changes to save."),
                  
                  br(),
                  
                  div("NHL Shot Chart Generator was created by Max Campbell.")
                )    
      )
      
    )
  )
)
  

server <- function(input, output, session) {
  
  ### SETTINGS ###
  #If it exists, get the timezone cookie on app startup
  observe({
    timezone_cookie <- get_cookie("timezone", missing = "America/New_York")
    updateSelectInput(session, "timezone", selected = timezone_cookie)
    updateDateInput(session, "date", value = getDate(timezone = timezone_cookie))
  })
  
  #Update default timezone whenever the preferred timezone is changed in the UI.
  observeEvent(input$save_settings, {
    
    set_cookie(
      cookie_name = "timezone",
      cookie_value = input$timezone
    )
    
  })
  
  ### SHOT CHART GENERATOR ###
  
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
  
  ### TABLE DISPLAY ###
  cleanedShotData <- reactive(
    df <- tryCatch ({
      getCleanedShotData(game_info()[input$game,]) |> #Can remove this pipeline to view the raw shotdata for debugging, if necessary.
        select(number, periodType, timeRemaining, situationCode, typeDescKey, x.Coord, y.Coord,
               shotType, shotDistance, shotAngle, awayScore, homeScore, awaySOG, homeSOG, firstName, lastName, TeamCode)
    }, error = function(e) {
      data.frame(error = "Shot data failed to generate! This error occurs if a game has not started yet or if shot data is incomplete early in a game.")
    })
  )
  
  output$table <- renderTable(cleanedShotData())
  
  output$exportCSV <- downloadHandler(
    filename = "ShotData.csv",
    content = function(file) {
      write.csv(cleanedShotData(), file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)