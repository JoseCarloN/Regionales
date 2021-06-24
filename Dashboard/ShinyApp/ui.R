library(dplyr)
library(shiny)
library(shinydashboard)
library(highcharter)
library(shinyjs)

ui = dashboardPage(
  
  dashboardHeader(title = "Indicadores Regionales", titleWidth = 300),
  
  
  dashboardSidebar(),
  
  
  dashboardBody(
    
    fluidRow(
      column(
        width = 6,
        highchartOutput("Heatmap")
      )
    )
    
    
  )
  
)
