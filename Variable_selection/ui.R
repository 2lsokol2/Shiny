library(shiny) # load the shiny package

work_data <- readRDS(file = "work_data.rds")

# Define UI for application
shinyUI(fluidPage(
  
  # Header or title Panel 
  titlePanel(h2('Cheack variable stability', align = "center")),
  
  # Sidebar panel
  sidebarPanel(
    
    
    selectInput("var", label = "1. Select Variable", 
                choices = names(work_data)[c(2, 4 , 5 , 7 , 8, 10, 11 ,13 ,15 ,16 ,18, 20, 21)],
                selected = 'chk_ac_status_1'),
    
    selectInput("dataset", label = "2. Select dataset", 
                choices = c("Train" = 1, "Train1" = 2, "Train2" = 3, "Test"=4),
                selected = 1)
    
  ),
  
  # Main Panel
  
  mainPanel(
    tabsetPanel(type="tab", 
                tabPanel("Plot", plotOutput("plot")),
                tabPanel("Summary",verbatimTextOutput("sum")),
                tabPanel("Structure", verbatimTextOutput("str"))
                
    ))))
