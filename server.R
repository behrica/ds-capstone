library(stringr)
library(shiny)
source("predict.R")

dts <- readRDS("dts.rds")

predictions <- data.frame(word=character(0),prob=numeric(0),stringsAsFactors = F)


shinyServer(function(input, output) {


  prediction <- reactive({
    predict<-predictNext(dts,str_trim(input$phrase))
    predict
  })

  output$prediction <- renderText({
    prediction()$word

  })

  output$prob <- renderText({
    paste(" (",prediction()$prob," %  confidence)")
  })

  output$probUI <- renderUI({
    style <- "color:#CC9900"
    if (prediction()$prob < 25)
      style <- "color:red"
    if (prediction()$prob > 75)
      style <- "color:green"

    span(style=style,
    uiOutput("prob",inline = T))
  })

 output$plot <- renderPlot({
   predictions[nrow(predictions)+1,] <<-  list(prediction()$word,prediction()$prob)
   predictions.show <- tail(predictions[! duplicated(predictions),],10)
   barplot(predictions.show$prob,names.arg = predictions.show$word,main = "Confidence of last 10 predictions")
 })

})
