library(dplyr)

shinyServer(function(input, output, session) {

  defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#FFD700", 
                     "#dd4477", "#000080", "#A0522D", "#00FF00")
  series <- structure(
    lapply(defaultColors, function(color) { list(color=color) }),
    names = levels(data$playlist_name)
  )

  yearData <- reactive({
    req(input$week_num) 
    req(input$content_type_2)
    # Filter to the desired year, and put the columns
    # in the order that Google's Bubble Chart expects
    # them (name, x, y, color, size). Also sort by region
    # so that Google Charts orders and colors the regions
    # consistently.
    df <- data %>%
      mutate(return_users = round(data$cond_prob_return*data$unique_users)) %>%
      filter(week_num == input$week_num) %>%
      select(episode_name, completion_rate, series_reach, playlist_name, return_users, 
             unique_users, content_type_2, talent_tier, week_num) %>%
      arrange(playlist_name)
   
     # Optional: filter by content type
    if (input$content_type_2 != "All") {
      df <- df %>% 
        filter(content_type_2 == input$content_type_2 )
    }
    
      # Optional: filter by talent tier
    if (input$talent_tier != "All") {
      df <- df %>% 
        filter(talent_tier == input$talent_tier )
    }
    
    # Optional: filter by playlist name
    if (!is.null(input$playlist_name) && input$playlist_name != "") {
      df <- df %>% 
        filter(playlist_name == input$playlist_name)
    }
    df
  })

  output$chart <- reactive({
    req(yearData())
    # Return the data and options
    list(
      data = googleDataTable(yearData()),
      options = list(
        title = sprintf(
          "Series Breadth vs. Completion Rate, Week %s",
          input$week_num),
        series = series
      )
    )
  })
})
