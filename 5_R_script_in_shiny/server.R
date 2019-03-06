shinyServer(
  function(input, output) {

    output$map <- renderPlot({

        percent_map( # some arguments )
    })

  }
)
