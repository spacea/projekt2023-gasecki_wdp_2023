library("shiny")

shinyUI(fluidPage(
  titlePanel("VoivodWeather"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "wybraneWoj", 
                  label = "Wybierz województwo",
                  choices = c("Dolnośląskie", "Mazowieckie", "Wielkopolskie"),
                  selected = "Wielkopolskie"),
    ),
    
    mainPanel(
      textOutput("woj"),
      textOutput("test"),
      textOutput("testb")
    )
  )
))