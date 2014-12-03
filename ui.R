
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


shinyUI(fluidPage(
  titlePanel("Next Word Predictor"),
  textInput("phrase","Phrase: "),
  tags$style(type='text/css', "#phrase { width: 500px; }")
  ,
  div("Prediction: ",
  strong(textOutput("prediction",inline=T)),
  uiOutput("probUI",inline = T),
  p(),
  plotOutput("plot")
  ))

)

