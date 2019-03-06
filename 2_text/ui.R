# runApp("S:/PROJECTS/! Individual/LSokol/!Rstudio/shiny", display.mode = "showcase")

library(shiny)

shinyUI(fluidPage(
  titlePanel(h1(strong("My Shiny App"))),
  sidebarLayout(
    sidebarPanel(
      h2(strong('Installation')),
      h6("Shiny is avaliable for CRAN, so...:", align = "center"),
      br(),
      code("code displays your text similar to computer code"),
      br(),
      br(),
      br(),
    img(src="bigorb.png", height = 100, width = 100),
      h6("shiny is a product of"),
      span("groups of words", style = "color:blue")),
    
    mainPanel(
      h1("Introduction Shiny"),
      p("A new p() command starts a new paragraph.", style = "font-family: 'times'; font-si16pt"),
      strong("strong() makes bold text."),
      em("em() creates italicized (i.e, emphasized) text."),
      div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
      br(),
      p("span does the same thing as div, but it works with",
        span("groups of words", style = "color:blue"),
        "that appear inside a paragraph."),
      br(),
      a("Shiny homepage.", 
        href = "http://www.rstudio.com/shiny")
    )
  )
))


