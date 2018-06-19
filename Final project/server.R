
#server.R
library(shiny)

function(input, output) {
  
output$value <- renderPrint({ input$dates })
  
}
