library(shiny)
library(DT)
library(shinyalert)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  tags$head(
    tags$style(HTML("
                    * {
                    font-family: Palatino,garamond,serif;
                    font-weight: 500;
                    line-height: 1.2;
                    #color: #000000;
                    }
                    "))
  ), 
  
  # Application title
  titlePanel("Sampling Distribution of Sample Mean"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      useShinyalert(),
      fileInput(inputId = "file", 
                "Upload a population data file",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      numericInput("n", "Sample size:", value = NULL),
      actionButton("draw", "Draw a sample", width = '100%'),
      HTML("<p>&nbsp; &nbsp; &nbsp; </p>"),
      numericInput("xbar", "Report the sample mean:", value = NULL),
      actionButton("submit", "Submit", width = '100%'),
      checkboxInput("class", "Show the Class Report", FALSE)
      #actionButton("report", "Class Report", width = '49%') 
      #actionButton("reset", "Reset", width = '31%')
    ),
  
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        column(width = 5, htmlOutput("text1")),
        column(width = 7, htmlOutput("text2")),
        ),
      fluidRow(
        column(width = 5, tableOutput("population")),
        column(width = 7, 
               tableOutput("sample"),
               plotOutput("graph")
               )
        )
        )#end of mainPanel
)#end of sidebar layout
)#end of fluidpage
)#end of shinyUI
