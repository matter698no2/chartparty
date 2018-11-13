# Version 2.0 of the Chart Party baseball
# statistics application
# 
# Designed to work only with Trackman .csv files
# 
# Notable Changes:
#  - Data Entry page
#  - Improved Aesthetics
#  - Improved Organization
#  - More space for future additions
# 
# Created by Matthew Rogers
# VT Statistics Undergradute
# Check Github for more information about version differences

library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(data.table)
source('functions.r')


  
# Be sure to change this if you rename the folder
# this is important, and will break WP() if changed
setwd("~/Documents/Baseball Scrimmages")

### WIN PROBABILITY DATASET ###
wpDataset <-data.table::fread("master_pbp.csv")

ui <- fluidPage(
  useShinyjs(),
  
  # get the CSS necessary
  includeCSS("www/app2Stylesheet.css"),
  
  # 1. Data Entry Page
  tags$div(id = "dataEntry",
            tags$div(id = "left",
                    h1("Chart Party"),
                    p("Created by Matthew Rogers")
                    ),
           tags$div(id = "right",
                    h3("Data Entry"),
                    p("Select the trackman dataset to work with"),
                    fileInput("phil", label = "", multiple = F),
                    actionButton("start", "Let's Go")
                    )
  ),
  
  shinyjs::hidden(
    tags$div(id = "restOfTheFuckenApp",
          tags$div(id = "title", titlePanel("Chart Party")),
          tabsetPanel(type = "tabs",
                      
                      ### Team Overview ###
                      tabPanel("Team Overview",
                               sidebarLayout(
                                 sidebarPanel(
                                   # importing data
                                   # fileInput("phil", label = "datIn", multiple = F),
                                   uiOutput("teamOptions")
                                 ),
                                 mainPanel(
                                   tableOutput("teamBatting")
                                 )
                               )),
                      
                      ### Pitchers Tab ###
                      tabPanel("Pitchers",
                               sidebarLayout(
                                 sidebarPanel(
                                   h3("Options"),
                                   helpText("Data is uploaded in the Team Overview Tab"),
                                   # changing the chart type
                                   radioButtons("chartType", "Chart Type", c("Scatter Plot", "Boxplot", "Bargraph")),
                                   # dataset filtering, see the UIoutput in the server function
                                   uiOutput("pitcherOptions")
                               ),
                               
                               mainPanel(
                                 # reactive plot of user's choosing
                                 plotOutput("baseballPlot"),
                                 # plot of the number of different pitches thrown
                                 plotOutput("pitchTotals"),
                                 # data summary
                                 verbatimTextOutput("ballsStrikes"),
                                 tableOutput("outcomeTotals"),
                                 plotOutput("pitchLoc"),
                                 plotOutput("relChart")
                               )
                             )
                    ),
                    
                    ## Hitters Tab ###
                    tabPanel("Hitters",
                             sidebarLayout(
                               sidebarPanel(
                                 h3("Options"),
                                 helpText("Data is uploaded in the Team Overview Tab"),
                                 uiOutput("hitterOptions")
                               ),
                                mainPanel(
                                 textOutput("hitterTitle"),
                                 tableOutput("launchAngleTab"), # a table with various launch angle stats
                                 tableOutput("exitVeloTab"), # a table with various exit velo stats
                                 # verbatimTextOutput("dayNums"), # the hitter's line for the game
                                 # verbatimTextOutput("plateDis"), # Oswing% & Zswing%
                                 tableOutput("elbinCharter")
                               )
                             )
                    ),
                    tabPanel("Report Generation",
                             titlePanel(h2("Report Options")),
                             radioButtons("reportType", "Report Type", choices = c("Team Batting", "Team Pitching", "Ind Pitching", "Ind Batting"))
                    ),
                    tabPanel("Win Probablity",
                             sidebarLayout(
                               sidebarPanel(
                                 h3("Situation Input"),
                                 helpText("Data comes from 2017 and 2018 pbp data"),
                                 uiOutput("wpOption"),
                                 helpText("Score difference is Home Score - Away Score. If greater than 10 or less than -10, just put 10 or -10")
                               ),
                               mainPanel(
                                 tags$div(id = "wpH2", h1("Win Probability:")),
                                 textOutput("wpOut"),
                                 tags$div(id = "wpSummary", 
                                          textOutput("nTimes"),
                                          textOutput("nWins")
                                          )
                               )
                             ))
          )
      )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$start, {
    shinyjs::toggle("dataEntry")
    shinyjs::toggle("restOfTheFuckenApp")
  })
  
  # Organization:
  # - Datasets
  # - Team Tab
  # - Pitcher Tab
  # - Hitter Tab
  
  ############################################
  #           Datasets
  ############################################
  
  ### COMPLETE DATASET ###
  dataset <- reactive({
    # takes the fileinput and turns it into a reactive dataset
    readFile <- input$phil # get the file input from above
    if (is.null(readFile)) { # check to make sure it isn't null
      return(NULL)
    }
    p <- read.csv(readFile$datapath, header = T) # read in the file as a csv from the temp path
    p$AngleCats <- cut(p$Angle, 
                       breaks = c(-Inf, 10, 20, 30, Inf),
                       labels = c("< 10", "10-20 (w/ 10)", "20-30 (w/ 20)", "> 30"),
                       right = F)
    
    p$exitVeloCats <- cut(p$ExitSpeed,
                          breaks = c(-Inf, 75, 85, 95, 105, Inf),
                          labels = c("< 75", "75-85", "85-95", "95-105", "> 105"),
                          right = F)
    
    return(p)
  })
  
  ### TEAM DATASET ###
  teamDataset <- reactive({
    d <- dataset()
    if(input$team != "") {
      d <- d %>%
        filter(BatterTeam == input$team)
    }
    
    return(d)
  })
  
  ### PITCHER DATASET ###
  subDataset <- reactive({
    # used exclusively with the pitcher tab
    d <- dataset()
    if (input$player != "") { # player name filter
      d <- d %>%
        filter(Pitcher == input$player)
    }
    if (input$pitchType != ""){ # pitch type filter
      d <- d %>%
        filter(TaggedPitchType == input$pitchType)
    }
    return(d)
  })
  
  ### HITTER DATASET ###
  hitterDataset <- reactive({
    # used exclusively with the hitter tab
    d <- dataset()
    if (input$hitterSelect != "") { # player name filter
      d <- d %>%
        filter(Batter == input$hitterSelect)
    }
    if (input$splitSelect != "") { # left/right split selection
      d <- d %>%
        filter(PitcherThrows == input$splitSelect)
    }
    if (input$pitchName != ""){ # pitch type filter
      d <- d %>%
        filter(TaggedPitchType == input$pitchType)
    }
    return(d)
  })
  
  

  
  
  ############################################
  #           Team Tab
  ############################################
  
  # uiOut for data filtering
  output$teamOptions <- renderUI({
    tagList(
      selectizeInput("team", "Hitter Team", choices = dataset()$PitcherTeam,
                     options = list(
                       placeholder = "",
                       onInitialize = I('function() { this.setValue(""); }')
                     ))
    )
  })
  
  output$teamBatting <- renderTable({batterTabGen(teamDataset())})
  
  ############################################
  #           Pitcher tab
  ############################################
  
  # Pitcher filtering options
  output$pitcherOptions <- renderUI({
    tagList(
      selectizeInput("player", label = "Pitcher", choices = dataset()$Pitcher,
                     options = list(
                       placeholder = "",
                       onInitialize = I('function() { this.setValue(""); }')
                     )), # pitcher name
      selectInput("xAxis", label = "xSelect", choices = names(dataset())), # x axis choices
      selectInput("yAxis", label = "ySelect", choices = names(dataset())), # y axis choices
      selectizeInput("pitchType", label = "Pitch Select",
                     choices = dataset()$TaggedPitchType,
                     options = list(
                       placeholder = "",
                       onInitialize = I('function() { this.setValue(""); }')
                     ))
    )
  })
  
  # Baseball Plot
  output$baseballPlot <- renderPlot(
    # Plot
    ggplot(data = subDataset(), aes_string(x = input$xAxis, y = input$yAxis))
    # change the type of plot based on the radio button input
    + switch(input$chartType,
             "Scatter Plot" = geom_point(),
             "Boxplot" = geom_boxplot(),
             "Bargraph" = geom_bar()
    )
    + ggtitle(paste("Player: ", toString(input$player)))
    # add the lob if a scatter plot is selected
    + switch(input$chartType,
             "Scatter Plot" = geom_smooth(method = "lm",
                                          se = F))
  )
  
  # pitch count output
  output$pitchTotals <- renderPlot(
    # a barplot of the counts for each pitch thrown
    ggplot(data = subDataset(), aes(TaggedPitchType)) + geom_bar()
  )
  
  # UI outputs
  output$ballsStrikes <- renderText({paste("Strikes:", toString(isStrikeCounter(subDataset()$PlateLocSide, subDataset()$PlateLocHeight)))})
  output$outcomeTotals <- renderTable({table(subDataset()$PitchCall)})
  
  # Strike Zone visualizer
  output$pitchLoc <- renderPlot({
    
    ggplot(subDataset(), aes(subDataset()$PlateLocSide, subDataset()$PlateLocHeight, color = subDataset()$PitchCall)) + geom_point() +
      geom_rect(data = NULL, mapping = aes(xmin = -0.7803, xmax = 0.7803, ymin = 1.5, ymax = 3.5), alpha = 0, color = "black") +
      xlim(-5, 5) +
      ylim(0, 6)
  })
  
  # Release Point Plot
  output$relChart <- renderPlot ({
    
    ggplot(subDataset(), aes(subDataset()$RelSide, subDataset()$RelHeight, color = subDataset()$TaggedPitchType)) + geom_point() +
      xlim(-4, 4) + ylim(0, 6)
    
  })
  
  ############################################
  #           Hitter tab
  ############################################
  
  # hitter dataset filtering options
  output$hitterOptions <- renderUI({
    tagList(
      selectizeInput("hitterSelect", label = "Batter Select",
                     choices = dataset()$Batter,
                     options = list(
                       placeholder = "",
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      selectizeInput("splitSelect", label = "L/R Splits",
                     choices = dataset()$PitcherThrows,
                     options = list(
                       placeholder = "",
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      selectizeInput("pitchName", label = "Pitch Select",
                     choices = dataset()$TaggedPitchType,
                     options = list(
                       placeholder = "",
                       onInitialize = I('function() { this.setValue(""); }')
                     ))
    )
  })
  
  # Title
  output$hitterTitle <- renderText(paste("Hitter: ", input$hitterSelect))
  
  # hitter Data outputs
  output$launchAngleTab <- renderTable({laTable(hitterDataset())})
  output$exitVeloTab <- renderTable({evTable(hitterDataset())})
  output$elbinCharter <- renderTable({elbinChart(hitterDataset())})
  
  ############################################
  #           Win Probability Tab
  ############################################
  
  # situational input
  output$wpOption <- renderUI({
    tagList(
      radioButtons("r1_in", "First Base", choices = c(0, 1), selected = NULL, inline = T),
      radioButtons("r2_in", "Second Base", choices = c(0, 1), selected = NULL, inline = T),
      radioButtons("r3_in", "Third Base", choices = c(0, 1), selected = NULL, inline = T),
      radioButtons("outs_in", "Outs", choices = c(0, 1, 2), selected = NULL, inline = T),
      numericInput("inning_select", "Inning", value = 1, min = 1, max = Inf),
      radioButtons("top_bottom", "Top or Bottom", choices = c(1, 0), selected = NULL, inline = T),
      numericInput("batting_order", "Batting Order", value = 1, min = 1, max = 9),
      numericInput("score_diff_in", "Score Difference", value = 0, min = 0, max = Inf)
    )
  })
  
  # win probability output
  output$wpOut <- renderText(WP(make_baseout(as.numeric(input$r1_in), as.numeric(input$r2_in), as.numeric(input$r3_in), as.numeric(input$outs_in)), input$inning_select, input$top_bottom, input$batting_order, input$score_diff_in))
  output$nTimes <- renderText("Teams have been in this situation x times")
  output$nWins <- renderText("The Home team has won x of those times")
  
  # stop the app when the window is closed
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)

