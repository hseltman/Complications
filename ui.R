# Purpose: Shiny user interface for Medical Complications
# Author: H. Seltman
# Date: Dec. 2017
# Reference: http://shiny.rstudio.com

library(shiny, warn.conflicts=FALSE, quietly=TRUE)
library(shinyjs, warn.conflicts=FALSE, quietly=TRUE)

# This is the user interface function, and it's value
# is the return value of sourcing this file.
fluidPage(
  titlePanel("Complication Rates"),
  shinyjs::useShinyjs(),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fileRef", "Download your csv file", accept="text/csv"),
      textOutput("fileMsg", inline=TRUE),
      hr(),
      shinyjs::disabled(
        radioButtons("analysisType", "Analysis type:",
                     c("All years", "Compare years", "One year","Year to date"))
      ),
      shinyjs::hidden(
        sliderInput("yearStart", "Starting month:", min=1, max=12, value=1,
                    step=1, sep="")
      ),
      span(id="instructions",
        hr(),
        p("Instructions:"),
        p("In Mac Numbers, use 'File / Export' to convert your file to 'csv'.",
          "Then use the widget above to load the file.")
      )
    ),
    
    mainPanel(
      verbatimTextOutput("stats"),
      plotOutput("plot"),
      dataTableOutput("tbl")
    )
  )
)
