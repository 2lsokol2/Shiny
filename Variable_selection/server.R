library(shiny)

# Restore the object
work_data <- readRDS(file = "work_data.rds")

source("helpers.R")

shinyServer(
  function(input, output) {
    
    dataset <- reactive({
      as.numeric(input$dataset) 
    })
    
    colm <- reactive({
      as.character(input$var)
    })
 
    output$dataset <- renderText({ 
      paste("Data set is", ifelse(dataset()==1, 'Train',
                                  ifelse(dataset()==2, 'Train1',
                                         ifelse(dataset()==3, 'Train2', 'Test'))))
      
    })
    

    output$plot <- renderPlot({
        var_stability(colm(), dataset())
    })
    
    output$sum <- renderPrint({
      summary(work_data)
      
    })
    
    output$str <- renderPrint({
      glimpse(work_data)
      
    })
    
})


