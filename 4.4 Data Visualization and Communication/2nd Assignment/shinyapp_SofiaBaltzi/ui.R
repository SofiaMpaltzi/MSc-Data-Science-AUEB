library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Cases/ Deaths per continent over time"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    selectInput(inputId = "userInput", label = "Choose a continent:", 
                choices = c( "Africa", "America", "Asia", "Europe", "Oceania", "All"))
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput(outputId = "barchart1"),
    plotOutput(outputId = "barchart2"),
  )
))
