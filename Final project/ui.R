
library(shiny)
library(leaflet)


fluidPage(
  
  titlePanel("Taiwan Dengue Historical Map"),
  
  sidebarLayout(
    
    sidebarPanel(
      radioButtons("dist", "Infected Region",
                   c("Republic of China" = "dengueloc",
                     "Other Countries" = "denguefor",
                    )),
      br(),
      
    sliderInput("n",
                "Year:",
                value = 2015,
                min = 2013,
                max = 2017)

  ),
  
  
  tabsetPanel(type = "tabs",
              tabPanel("Plot", plotOutput("plot")),
              tabPanel("Summary", verbatimTextOutput("summary")),
              tabPanel("Table", tableOutput("table"))
  )
  
  )
)

