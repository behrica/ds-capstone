library(stringr)
library(shiny)
source("predict.R")

dts <- readRDS("dts.rds")





shinyServer(function(input, output) {
  
  output$prediction <- renderText({
    prediction <- predictNext(dts,str_trim(input$phrase))
    paste("Next word prediction: ",prediction)
    
  })
  
})
