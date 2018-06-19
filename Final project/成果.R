library(shiny)
library(leaflet)


ui <- fluidPage(
  
  titlePanel("Taiwan Dengue Historical Map"),
  
  sidebarLayout(
    
    sidebarPanel(
      radioButtons(inputId = "region", label = "Infected Region",
                   choices = c("Republic of China" = "1",
                               "Other Countries" = "0")
      ),
      br(),
      
      sliderInput("n",
                  "Year:",
                  value = 2015,
                  min = 2013,
                  max = 2017)
      
    ),
    
    
    tabsetPanel(type = "tabs",
                tabPanel("Plot", leafletOutput(outputId = "plot")),
                tabPanel("Table", tableOutput("table"))
                )
  )
  

)

server <- function(input, output){
  current_region <- reactive({
    filter(dengue, isloc==input$region)
  })
  current_year <- reactive({
    filter(current_region(), Year==input$n)
  })
  
  output$plot <- renderLeaflet({
    leaflet(current_year()) %>% 
      addTiles() %>% 
      addCircleMarkers(~current_year()$lon, ~current_year()$lat, radius = 1, fillOpacity = 0.5)
  })
  
  output$table <- renderTable({
     current_year()
  })
}

shinyApp(ui = ui, server = server)