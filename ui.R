install.packages("shiny")
library("shiny")

shinyUI(fluidPage(
  titlePanel("VoivodWeather"),
  sidebarLayout(
    sidebarPanel(
      p("Input controls")
    ),
    mainPanel(
      br(),
      p("Outputs")
    )
  )
))