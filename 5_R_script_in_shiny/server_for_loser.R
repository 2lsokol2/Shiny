library(shiny)
library(maps)
library(mapproj)
source("helpers.R")
counties <- readRDS("data/counties.rds")


shinyServer(
  function(input, output) {
    output$map <- renderPlot({
      data <- switch(input$var, 
                     "Percent White" = counties$white,
                     "Percent Black" = counties$black,
                     "Percent Hispanic" = counties$hispanic,
                     "Percent Asian" = counties$asian)
      
      palitra <- switch(input$var,
                      "Percent White" = "darkgreen",
                     "Percent Black" = "black",
                     "Percent Hispanic" = "blue",
                     "Percent Asian" = "azure")
      
      title_upper <- switch(input$var,
                        "Percent White" = "Norm",
                        "Percent Black" = "Nigers",
                        "Percent Hispanic" = "Bulls-likers",
                        "Percent Asian" = "Narrow eyes")
      
 
      percent_map(var = data, 
                  color = palitra, 
                  legend.title = title_upper, 
                  max = input$range[2], 
                  min = input$range[1])
    })
  }
)
