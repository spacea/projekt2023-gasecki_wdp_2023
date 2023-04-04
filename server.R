source("czystopis.R")

shinyServer(function(input, output) {
  output$stacje = renderTmap({
    input$sgen
    isolate(map_woj(woj = input$swoj,
                    year = input$syear,
                    mon = input$smon,
                    day = input$sday,
                    rank = input$srank))
  })
  
  output$klimatogram = renderPlot({
    input$gen
    isolate(klim_woj(input$woj, 
                     input$year,
                     input$rank))
  })
})
