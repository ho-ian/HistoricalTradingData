
library(shiny)
library(plotly)

yahooticks = "Yahoo-Ticker-Symbols-September-2017.csv"
tickers = read.csv(yahooticks)
tickers = tickers[4:nrow(tickers), "Yahoo.Stock.Tickers"]

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
                                selectizeInput("tickersA", "Stock Tickers", choices = tickers, selected="AAPL"),
                                uiOutput("timeA"),
                                sliderInput("A_r", "Red", min=0, max=255, value=150),
                                sliderInput("A_g", "Green", min=0, max=255, value=150),
                                sliderInput("A_b", "Blue", min=0, max=255, value=255),
                                offset=1
                         ),
                         conditionalPanel("input.comparison==true",
                                          column(4,
                                                 h4("Stock Ticker B"),
                                                 selectizeInput("tickersB", "Stock Tickers", choices = tickers, selected="GOOGL"),
                                                 uiOutput("timeB"),
                                                 sliderInput("B_r", "Red", min=0, max=255, value=235),
                                                 sliderInput("B_g", "Green", min=0, max=255, value=150),
                                                 sliderInput("B_b", "Blue", min=0, max=255, value=0)
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
