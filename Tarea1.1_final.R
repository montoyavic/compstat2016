library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(plotly)
library(MASS)

# Funcion Inversa
ExpInv <- function(n.sims, lambda){
  u <- runif(n.sims)
  -(log(1-u))*(lambda^(-1))
}

ui<-fluidPage(
  
  # Application title
  headerPanel("Estadistica Computacional Tarea"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("sim", 
                "Numero de simulaciones:", 
                min = 10,
                max = 10000, 
                value = 100)
  ),
  sidebarPanel(
    sliderInput("par", 
                "Parametro:", 
                min = 1,
                max = 10, 
                value = 1)
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot")
  )
)

server<- function(input, output) {
  
  
  output$distPlot <- renderPlot({
    
    # llamado funcion
    dist <- ExpInv(input$sim,input$par )
    hist(dist)
  })
}

shinyApp(ui = ui, server = server)
