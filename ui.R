
library(shiny)
library(plotly)

shinyUI(fluidPage(
  
  titlePanel("Historical Trading Data"),
  
  tabsetPanel(type="tabs",
              tabPanel("Stock Info",
                       hr(),
                       fluidRow(
                         column(6,plotlyOutput("radarPlot")),
                         column(6, plotlyOutput("xtsInfo"))
                       ),
                       
                       
                       fluidRow(
                         column(4,
                                h4("Stock Ticker A"),
                                uiOutput("tickersA"),
                                uiOutput("timeA"),
                                uiOutput("colorA_r"),
                                uiOutput("colorA_g"),
                                uiOutput("colorA_b"),
                                offset=1
                         ),
                         conditionalPanel("input.comparison==true",
                                          column(4,
                                                 h4("Stock Ticker B"),
                                                 uiOutput("tickersB"),
                                                 uiOutput("timeB"),
                                                 uiOutput("colorB_r"),
                                                 uiOutput("colorB_g"),
                                                 uiOutput("colorB_b")
                                          )
                         ),
                         
                         column(3,
                                checkboxInput("comparison", HTML("<h5><strong>Compare Tickers?</strong></h5>"), FALSE)
                         ),
                         column(1)
                       )),
              tabPanel("About",
                       htmlOutput("aboutme")
              ),
              tabPanel("Resources",
                       htmlOutput("resources")
              )
              
  )
  
  
))
