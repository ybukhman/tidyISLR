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

# Only look at the players whose salaries have been recorded
Hitters2 <- filter(Hitters, is.finite(Salary))

# Add the Rookie variable
Hitters2 <- mutate(Hitters2, Rookie = ifelse(Years < 4, TRUE, FALSE))

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
      plotOutput("histPlot"),
      plotOutput("violinJitterPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # The variable to display
  values <- reactive({Hitters2[,input$variable]})
  
  # Histogram
  output$histPlot <- renderPlot({
    
    # The vector to plot
    x <- values()
    
    #x <- Hitters2[,input$variable]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = input$variable,
         main = paste('Histogram of', input$variable))
  })
  
  # Violin + jitter plot
  output$violinJitterPlot <- renderPlot({
    ggplot(Hitters2) + 
      geom_violin(aes(x = Rookie, y = values())) +
      geom_jitter(aes(x = Rookie, y = values())) +
      scale_y_log10() +
      ylab(input$variable)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
