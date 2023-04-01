source("test.R")
# source("project.R")
# source("wojewodztwa.R")
# source("czystopis.R")

shinyServer(function(input, output) {
  output$woj = renderText(input$wybraneWoj)
  output$test = renderText(test())
})