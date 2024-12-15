#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(ISLR2)
library(tidyverse)
library(ggplot2)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Hitters Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          # Input: Selector for choosing dataset ----
          selectInput(
            inputId = "variable",
            label = "Choose a variable:",
            choices = names(Hitters)
          ),
          
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Only look at the players whose salaries have been recorded
  Hitters2 <- filter(Hitters, is.finite(Salary))

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- Hitters2[,input$variable]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = input$variable,
             main = paste('Histogram of', input$variable))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
