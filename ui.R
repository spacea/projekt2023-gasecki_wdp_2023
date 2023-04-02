library("shiny")

woj

shinyUI(fluidPage(
  titlePanel("VoivodWeather"),
  sidebarLayout(
    sidebarPanel(
      h3("Klimatogram"),
      selectInput(inputId = "woj", 
                  label = "Województwo",
                  choices = c("zpom", "kpom", "dol"),
                  selected = "zpom"),
      numericInput(inputId = "year",
                  label = "Rok",
                  value = 2022,
                  min = 1960,
                  max = 2022)
    ),
    
    
    mainPanel(
      textOutput("test"), 
      plotOutput("klimatogram")
    )
  )
))