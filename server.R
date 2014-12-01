library(stringr)
library(shiny)
source("predict.R")

dts <- readRDS("dts.rds")

shinyServer(function(input, output) {

  prediction <- reactive({
    predictNext(dts,str_trim(input$phrase))
  })

  output$prediction <- renderText({
    prediction()$word

  })

  output$prob <- renderText({
    paste(" (",prediction()$prob," %  )")
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



})
