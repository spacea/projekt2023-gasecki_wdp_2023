source("test.R")
source("czystopis.R")

shinyServer(function(input, output) {
  output$test = renderText(test(input$kupa))
  
  output$klimatogram = renderPlot(klim_woj(input$woj, input$year))
})