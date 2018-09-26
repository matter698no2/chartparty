# An app designed to work specifically with
# Trackman spreadsheets. Make sure to delete the
# "notes" section from the sheets before merging/
# uploading the data. 
# 
# Creator: Matthew Rogers

library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)

source('functions.r')

# Be sure to change this if you rename the folder
setwd("~/Documents/Baseball Scrimmages")

ui <- fluidPage(
  titlePanel("Chart Party"),
  tabsetPanel(type = "tabs",
  tabPanel("Team Overview",
     sidebarLayout(
       sidebarPanel(
         # importing data
         fileInput("phil", label = "datIn", multiple = F),
         uiOutput("teamOptions")
       ),
       mainPanel(
         verbatimTextOutput("teamBA"),
         plotOutput("overviewPlot")
         
       )
     )),
  tabPanel("Pitchers",
    sidebarLayout(
      sidebarPanel(
        h3("Options"),
        helpText("Data is uploaded in the Team Overview Tab"),
        # changing chart type
        radioButtons("chartType", "Chart Type", c("Scatter Plot", "Boxplot", "Bargraph")),
        # dataset filtering
        uiOutput("pitcherOptions")
      
        # Report Generation???
      ),
      mainPanel(
        # reactive plot of user's choosing
        plotOutput("baseballPlot"),
        # plot of the number of different pitches thrown
        plotOutput("pitchTotals"),
        # data summary
        verbatimTextOutput("playerName"),
        verbatimTextOutput("typePitch"),
        verbatimTextOutput("ballsStrikes"),
        verbatimTextOutput("avgY"),
        tableOutput("outcomeTotals"),
        plotOutput("pitchLoc")
        )
      )
    ),
    tabPanel("Hitters",
         sidebarLayout(
           sidebarPanel(
             h3("Options"),
             helpText("Data is uploaded in the Team Overview Tab"),
             uiOutput("hitterOptions")
           ),
           mainPanel(
              h2("Hitter Summary"),
              verbatimTextOutput("dayNums"), # the hitter's line for the game (e.g. 2-4, 1-2, includes PAs)
              verbatimTextOutput(""),
              verbatimTextOutput("abLen") # the total number of pitches seen (e.g. 25 pitches seen, avg = 4)
           )
         )    
    )
  )
)

server <- function(input, output, session) {
  # --- Full Dataset --- #
  # This just turns the uploaded file into a 
  # reactive dataset
  dataset <- reactive({
      readFile <- input$phil # get the file input from above
      if (is.null(readFile)) { # check to make sure it isn't null
        return(NULL)
      }
      read.csv(readFile$datapath, header = T) # read in the file as a csv from the temp path
  })

  # filters for the teams specifically 
  teamDataset <- reactive({
    d <- dataset()
    if (input$team != "") {
      d <- d %>%
        filter(PitcherTeam == input$team)
    }
    return(d)
  })
  
  # filters for pitchers specifically
  subDataset <- reactive({
    d <- dataset()
    if (input$player != "") {
      d <- d %>%
        filter(Pitcher == input$player)
    }
    if (input$pitchType != ""){
        d <- d %>%
          filter(TaggedPitchType == input$pitchType)
    }
    return(d)
  })
  
  #filters for hitters specifically
  hitterDataset <- reactive({
    d <- dataset()
    if (input$hitterSelect != "") {
      d <- d %>%
        filter(Batter == input$hitterSelect)
    }
    if (input$splitSelect != "") {
      d <- d %>%
        filter(PitcherThrows == input$splitSelect)
    }
    return(d)
  })
  
  # Plot Renderer
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
  
  # pitchLoc Output
  output$pitchLoc <- renderPlot({
    
    ggplot(subDataset(), aes(subDataset()$PlateLocSide, subDataset()$PlateLocHeight, color = subDataset()$PitchCall)) + geom_point() + 
              geom_rect(data = NULL, mapping = aes(xmin = -0.7803, xmax = 0.7803, ymin = 1.5, ymax = 3.5), alpha = 0, color = "black") +
              xlim(-5, 5) +
              ylim(0, 6)
  })
  
  # uiOut for data filtering
  output$teamOptions <- renderUI({
    tagList(
      selectizeInput("team", "Team", choices = dataset()$PitcherTeam,
                     options = list(
                       placeholder = "",
                       onInitialize = I('function() { this.setValue(""); }')
                     ))
    )
  })
  
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
                     ))
      
    )
  })
  
  # team summary
  output$teamBA <- renderText({paste("Team BA:", toString(hits(teamDataset())/ab(teamDataset())) )})
  
  # pitcher summary
  output$playerName <- renderText({paste("Player Name:", toString(input$player))})
  output$typePitch <- renderText({paste("Pitch Type:", toString(input$pitchType))})
  output$ballsStrikes <- renderText({paste("Strikes:", toString(isStrikeCounter(subDataset()$PlateLocSide, subDataset()$PlateLocHeight)), "Balls:")})
  output$avgY <- renderText({paste("Average [whatever you selected] velo: ", toString(mean(subDataset()$RelSpeed)))})
  output$outcomeTotals <- renderTable({table(subDataset()$PitchCall)})
  
  # hitter summary
  output$dayNums <- renderText({paste(toString(hits(hitterDataset())), "for", toString(ab(hitterDataset())), "with", toString(pa(hitterDataset())), "plate appearences")})
  # stop the app when the window is closed
  session$onSessionEnded(function() {
    stopApp()
  })
}
# toString(nrow(subDataset()) - isStrikeCounter(subDataset()$PlateLocSide, subDataset()$PlateLocHeight))
shinyApp(ui, server)