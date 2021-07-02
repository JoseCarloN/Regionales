library (shinyjs)
library (tidyr)
library (data.table)
library (highcharter)
library (dplyr)
library (shinydashboard)
library (shiny)

x <- c("Farm","Farm","Farm","City","City","City","Ocean","Ocean")
y <- c("Sheep","Sheep","Cow","Car","Bus","Bus","Boat","Boat")
z <- c("Bill","Tracy","Sandy","Bob","Carl","Newt","Fig","Tony")
a <- c(1,1,1,1,1,1,1,1)

dat <- data.frame(x,y,z,a)

header <- dashboardHeader()
body <- dashboardBody(
  highchartOutput("Working"),
  verbatimTextOutput("trial")
  
)
sidebar <- dashboardSidebar()

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  output$Working <- renderHighchart({
    # Make the initial data.
    summarized <- dat %>%
      group_by(x) %>%
      summarize(Quantity = sum(a))
    
    summarized <- arrange(summarized, desc(Quantity))
    tibbled <- tibble(name = summarized$x, y = summarized$Quantity)
    
    # This time, click handler is needed.
    drilldownHandler <- JS("function(event) {Shiny.onInputChange('ClickedInput', event.point.drilldown);}")
    
    # Also a message receiver for later async drilldown data has to be set.
    # Note in the JS: message.point is going to be the point ID. Highcharts addSeriesAsDrilldown need a point to attach
    #   the drilldown series to. This is retrieved via chart.get which takes the ID of any Highcharts Element.
    #   This means: IDs are kind of important here, so keep track of what you assign.
    installDrilldownReceiver <- JS("function() {
      var chart = this;
      Shiny.addCustomMessageHandler('drilldown', function(message) {
        var point = chart.get(message.point)
        chart.addSeriesAsDrilldown(point, message.series);
      });
    }")
    
    highchart() %>%
      # Both events are on the chart layer, not by series. 
      hc_chart(events = list(load = installDrilldownReceiver, drilldown = drilldownHandler)) %>%
      hc_xAxis(type = "category") %>%
      # Note: We add a drilldown directive (= name) to tell Highcharts that this has a drilldown functionality.
      hc_add_series(tibbled, "column", hcaes(x = name, y = y, drilldown = name, id = name), color = "#E4551F") %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_drilldown(allowPointDrilldown = TRUE)
  })
  
  # Drilldown handler to calculate the correct drilldown
  observeEvent(input$ClickedInput, {
    # We will code the drill levels to be i.e. Farm_Car. By that we calculate the next Sub-Chart.
    levels <- strsplit(input$ClickedInput, "_", fixed = TRUE)[[1]]
    # This is just for generalizing this function to work in all the levels and even be expandable to further more levels.
    resemblences <- c("x", "y", "z")
    
    dataSubSet <- dat
    
    # We subsequently narrow down the original dataset by walking through the drilled levels
    for (i in 1:length(levels)) {
      dataSubSet <- dat[dat[[resemblences[i]]] == levels[i],]
    }
    
    # Create a common data.frame for all level names.
    normalized <- data.frame(category = dataSubSet[[resemblences[length(levels) + 1]]], amount = dataSubSet$a)
    
    summarized <- normalized %>%
      group_by(category) %>%
      summarize(Quantity = sum(amount))
    
    summarized <- arrange(summarized, desc(Quantity))
    
    tibbled <- tibble(name = summarized$category, y = summarized$Quantity)
    
    # Preparing the names and drilldown directives for the next level below.
    # If already in "Farm_Car", the name for column "Bob" will be "Farm_Car_Bob"
    nextLevelCodes = lapply(tibbled$name, function(fac) {
      paste(c(levels, as.character(fac)), collapse = "_")
    }) %>% unlist
    
    tibbled$id = nextLevelCodes
    
    # This is dynamic handling for when there is no further drilldown possible.
    # If no "drilldown" property is set in the data object, Highcharts will not let further drilldowns be triggered.
    if (length(levels) < length(resemblences) - 1) {
      tibbled$drilldown = nextLevelCodes
    }
    
    # Sending data to the installed Drilldown Data listener.
    session$sendCustomMessage("drilldown", list(
      series = list(
        type = "column",
        name = paste(levels, sep = "_"),
        data = list_parse(tibbled)
      ),
      # Here, point is, as mentioned above, the ID of the point that triggered the drilldown.
      point = input$ClickedInput
    ))
  })
  
  output$trial <- renderText({input$ClickedInput})
}

shinyApp(ui, server)