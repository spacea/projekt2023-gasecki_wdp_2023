source("voivodweather.R")

# Podstawowa funkcja pakietu Shiny.
# Zawiera funkcje renderujące mapę stacji i klimatogram w interfejsie.
shinyServer(function(input, output) {
  # Interaktywna mapa stacji.
  output$stacje = renderTmap({
    input$sgen
    isolate(map_woj(woj = input$swoj,
                    year = input$syear,
                    mon = input$smon,
                    day = input$sday,
                    rank = input$srank))
  })
  
  # Klimatogram.
  output$klimatogram = renderPlot({
    input$gen
    isolate(klim_woj(input$woj, 
                     input$year,
                     input$rank))
  })
})
