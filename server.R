library(stringr)
library(shiny)
library(ggplot2)
source("predict.R")
source("plotConfidence.R")

dts <- readRDS("dts.rds")

predictions <- data.frame(word=character(0),prob=numeric(0),stringsAsFactors = F)


shinyServer(function(input, output) {


  prediction <- reactive({
    if (str_sub(input$phrase,-1)==" ")
      return(predictNext(dts,str_trim(input$phrase)))
    else {
      if (nrow(predictions)==0) return(list(word="",prob=0))
      else return(tail(predictions,1))
    }

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
   predictions[nrow(predictions)+1,] <<-  list(word=prediction()$word,prob=prediction()$prob)
   dput(predictions,"")
   createConfidencePlot(predictions)
#    predictions.show <- tail(predictions[! duplicated(predictions),],10)
#    predictions.show$word <- make.unique(predictions.show$word)
#    predictions.show$word <- factor(predictions.show$word,predictions.show$word,ordered = T)
#
#    ggplot(predictions.show,aes(x = word,y=prob)) +
#      geom_bar(aes(fill=prob),stat="identity") +
#      scale_fill_gradient2(low = 'red', mid = 'yellow', high = 'green', midpoint = 50) +
#      coord_cartesian(xlim=c(0,12),ylim=c(0,100)) +
#      ggtitle("Confidence of last 10 predictions") +
#      ylab("Confidence") + xlab("predicted word") +
#      theme_minimal()

 })

})
