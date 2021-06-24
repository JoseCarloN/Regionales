library(dplyr)
library(shiny)
library(shinydashboard)
library(highcharter)
library(shinyjs)

viewer = getOption("viewer")
viewer("http://localhost:7693")

server = function(input, output) {
  
  indicadores = read.csv("Test.csv", encoding = "UTF-8")
  colnames(indicadores) = c("Indicador", colnames(indicadores[,-1]))
  
  stops = data.frame(breaks = c(0, 0.9, 1),
                     colors = c("#ee6352", "#80ff72", "#169873"),
                     stringsAsFactors = FALSE)
  stops = list_parse2(stops)
  
  output$Heatmap = renderHighchart({
    hchart(indicadores, type = "heatmap", hcaes(x = Region, y = Indicador, value = Avance), name = "Avance") %>%
      hc_colorAxis(stops = stops, startOnTick = FALSE, endOnTick = FALSE) 
  })

}