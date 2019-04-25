
library(shiny)
library(quantmod)
library(plotly)
library(xts)

yahooticks = "Yahoo-Ticker-Symbols-September-2017.csv"

shinyServer(function(input, output) {
  
  tickers = read.csv(yahooticks)
  tickers = tickers[4:nrow(tickers), "Yahoo.Stock.Tickers"]
  
  
  
  output$tickersA <- renderUI({
    if (input$comparison) {
      selectizeInput("tickersA", "Stock Tickers Group A", tickers, selected="AAPL")
    }
    else {
      selectizeInput("tickersA", "Stock Tickers" , tickers, selected="AAPL")
    }

  })
  
  output$tickersB <- renderUI({
    if (input$comparison) {
      selectizeInput("tickersB", "Stock Tickers Group B", tickers, selected="GOOGL")
    }
  })
  
  
  output$timeA <- renderUI({
    
    sym_xtsA = getSymbols(input$tickersA, src="yahoo", auto.assign=F)
    dates = index(sym_xtsA)
    sliderInput("dateA", "Date", 
                min=dates[1], max=dates[length(dates)], value=dates[1],
                animate = 
                  animationOptions(interval=1000, loop=TRUE))
  })
  
  output$timeB <- renderUI({
    if (input$comparison) {
      sym_xtsB = getSymbols(input$tickersB, src="yahoo", auto.assign=F)
      dates = index(sym_xtsB)
      sliderInput("dateB", "Date", 
                  min=dates[1], max=dates[length(dates)], value=dates[1],
                  animate = 
                    animationOptions(interval=1000, loop=TRUE))
    }
  })
  
  output$colorA_r <- renderUI({
    
    if (input$comparison) {
      sliderInput("A_r", "Red Group A", min=0, max=255, value=150)
    }
    else {
      sliderInput("A_r", "Red", min=0, max=255, value=150)
    }
  
  })


  output$colorA_g <- renderUI({
    
    if (input$comparison) {
      sliderInput("A_g", "Green Group A", min=0, max=255, value=150)
    }
    else {
      sliderInput("A_g", "Green", min=0, max=255, value=150)
    }
    
    
  })
  
  output$colorA_b <- renderUI({
    
    if (input$comparison) {
      sliderInput("A_b", "Blue Group A", min=0, max=255, value=255)
    }
    else {
      sliderInput("A_b", "Blue", min=0, max=255, value=255)
    }
    
  })
  
  output$colorB_r <- renderUI({
    
    if (input$comparison) {
      sliderInput("B_r", "Red Group B", min=0, max=255, value=235)
    }
    
  })
  
  
  output$colorB_g <- renderUI({
    
    if (input$comparison) {
      sliderInput("B_g", "Green Group B", min=0, max=255, value=150)
    }
    
    
  })
  
  output$colorB_b <- renderUI({
    
    if (input$comparison) {
      sliderInput("B_b", "Blue Group B", min=0, max=255, value=0)
    }
    
  })
  
   
  output$radarPlot <- renderPlotly({
    
    sym_xtsA = getSymbols(input$tickersA, src="yahoo", auto.assign=F)
    full_datesA = xts(order.by = seq(from = start(sym_xtsA), to = end(sym_xtsA), by= "day")) # fill in all the weekends and holidays not listed 
    sym_xtsA = merge(full_datesA, sym_xtsA, all=T) # add in the values that were recorded in the weekday and non-holidays
    sym_xtsA = na.locf(sym_xtsA) # fill in empty spaces with previous values, i.e. Saturday and Sunday would be filled in with Friday's values
    
    openA = paste(input$tickersA, "Open", sep=".")
    highA = paste(input$tickersA, "High", sep=".")
    lowA = paste(input$tickersA, "Low", sep=".")
    closeA = paste(input$tickersA, "Close", sep = ".")
    volA = paste(input$tickersA, "Volume", sep=".")
    adjA = paste(input$tickersA, "Adjusted", sep=".")
    
    sym_xtsA[,openA] = scale(sym_xtsA[,openA], center=T)
    sym_xtsA[,highA] = scale(sym_xtsA[,highA], center=T)
    sym_xtsA[,lowA] = scale(sym_xtsA[,lowA], center=T)
    sym_xtsA[,closeA] = scale(sym_xtsA[,closeA], center=T)
    sym_xtsA[,volA] = scale(sym_xtsA[,volA], center=T)
    sym_xtsA[,adjA] = scale(sym_xtsA[,adjA], center=T)
    
    if (input$comparison) {
      
      sym_xtsB = getSymbols(input$tickersB, src="yahoo", auto.assign=F)
      full_datesB = xts(order.by = seq(from = start(sym_xtsB), to = end(sym_xtsB), by= "day")) # fill in all the weekends and holidays not listed 
      sym_xtsB = merge(full_datesB, sym_xtsB, all=T) # add in the values that were recorded in the weekday and non-holidays
      sym_xtsB = na.locf(sym_xtsB) # fill in empty spaces with previous values, i.e. Saturday and Sunday would be filled in with Friday's values
      
      openB = paste(input$tickersB, "Open", sep=".")
      highB = paste(input$tickersB, "High", sep=".")
      lowB = paste(input$tickersB, "Low", sep=".")
      closeB = paste(input$tickersB, "Close", sep = ".")
      volB = paste(input$tickersB, "Volume", sep=".")
      adjB = paste(input$tickersB, "Adjusted", sep=".")
      
      sym_xtsB[,openB] = scale(sym_xtsB[,openB], center=T)
      sym_xtsB[,highB] = scale(sym_xtsB[,highB], center=T)
      sym_xtsB[,lowB] = scale(sym_xtsB[,lowB], center=T)
      sym_xtsB[,closeB] = scale(sym_xtsB[,closeB], center=T)
      sym_xtsB[,volB] = scale(sym_xtsB[,volB], center=T)
      sym_xtsB[,adjB] = scale(sym_xtsB[,adjB], center=T)
      
      radartitle = paste(input$tickersA, "and", sep=" ")
      radartitle = paste(radartitle, input$tickersB, sep=" ")
      radartitle = paste(radartitle, "Radar Chart", sep=" ")
      
      colorsA = "rgba(#R,#G,#B,1)"
      colorsA = gsub("#R", input$A_r, colorsA)
      colorsA = gsub("#G", input$A_g, colorsA)
      colorsA = gsub("#B", input$A_b, colorsA)
      
      colorsB = "rgba(#R,#G,#B,1)"
      colorsB = gsub("#R", input$B_r, colorsB)
      colorsB = gsub("#G", input$B_g, colorsB)
      colorsB = gsub("#B", input$B_b, colorsB)
      
      p = plot_ly(
        type = 'scatterpolar',
        fill = 'toself'
      ) %>%
        add_trace(
          r = c(coredata(sym_xtsA[input$dateA,openA]), coredata(sym_xtsA[input$dateA,highA]), coredata(sym_xtsA[input$dateA,lowA]),
                coredata(sym_xtsA[input$dateA,closeA]), coredata(sym_xtsA[input$dateA,volA]), coredata(sym_xtsA[input$dateA,adjA]),
                coredata(sym_xtsA[input$dateA,openA])),
          theta = c('Open','High','Low', 'Close', 'Volume', 'Adjusted','Open'),
          name=input$tickersA,
          col="green"
        ) %>%
        add_trace(
          r = c(coredata(sym_xtsB[input$dateB,openB]), coredata(sym_xtsB[input$dateB,highB]), coredata(sym_xtsB[input$dateB,lowB]),
                coredata(sym_xtsB[input$dateB,closeB]), coredata(sym_xtsB[input$dateB,volB]), coredata(sym_xtsB[input$dateB,adjB]),
                coredata(sym_xtsB[input$dateB,openB])),
          theta = c('Open','High','Low', 'Close', 'Volume', 'Adjusted','Open'),
          name=input$tickersB
        ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = F,
              range=c(-4,4)
            )
          ),
          showlegend = T,
          title=radartitle,
          colorway = c(colorsA, colorsB)
        )
      
    }
    
    else {
      radartitle = paste(input$tickersA, "Radar Chart", sep=" ")
      colorsA = "rgba(#R,#G,#B,1)"
      colorsA = gsub("#R", input$A_r, colorsA)
      colorsA = gsub("#G", input$A_g, colorsA)
      colorsA = gsub("#B", input$A_b, colorsA)
      
      
      
      p = plot_ly(
        type = 'scatterpolar',
        r = c(coredata(sym_xtsA[input$dateA,openA]), coredata(sym_xtsA[input$dateA,highA]), coredata(sym_xtsA[input$dateA,lowA]),
              coredata(sym_xtsA[input$dateA,closeA]), coredata(sym_xtsA[input$dateA,volA]), coredata(sym_xtsA[input$dateA,adjA]),
              coredata(sym_xtsA[input$dateA,openA])),
        theta = c('Open','High','Low', 'Close', 'Volume', 'Adjusted','Open'),
        fill ='toself'
      ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = F,
              range=c(-4,4)
            )
          ),
          showlegend = F,
          title=radartitle,
          colorway = c(colorsA)
        )
    }
    
    
  })
  
  output$xtsInfo <- renderPlotly({
    
    sym_xtsA = getSymbols(input$tickersA, src="yahoo", auto.assign=F)
    full_datesA = xts(order.by = seq(from = start(sym_xtsA), to = end(sym_xtsA), by= "day")) # fill in all the weekends and holidays not listed 
    sym_xtsA = merge(full_datesA, sym_xtsA, all=T) # add in the values that were recorded in the weekday and non-holidays
    sym_xtsA = na.locf(sym_xtsA) # fill in empty spaces with previous values, i.e. Saturday and Sunday would be filled in with Friday's values
    
    openA = paste(input$tickersA, "Open", sep=".")
    highA = paste(input$tickersA, "High", sep=".")
    lowA = paste(input$tickersA, "Low", sep=".")
    closeA = paste(input$tickersA, "Close", sep = ".")
    volA = paste(input$tickersA, "Volume", sep=".")
    adjA = paste(input$tickersA, "Adjusted", sep=".")
    
    
    
    if (input$comparison) {
      sym_xtsB = getSymbols(input$tickersB, src="yahoo", auto.assign=F)
      full_datesB = xts(order.by = seq(from = start(sym_xtsB), to = end(sym_xtsB), by= "day")) # fill in all the weekends and holidays not listed 
      sym_xtsB = merge(full_datesB, sym_xtsB, all=T) # add in the values that were recorded in the weekday and non-holidays
      sym_xtsB = na.locf(sym_xtsB) # fill in empty spaces with previous values, i.e. Saturday and Sunday would be filled in with Friday's values
      
      openB = paste(input$tickersB, "Open", sep=".")
      highB = paste(input$tickersB, "High", sep=".")
      lowB = paste(input$tickersB, "Low", sep=".")
      closeB = paste(input$tickersB, "Close", sep = ".")
      volB = paste(input$tickersB, "Volume", sep=".")
      adjB = paste(input$tickersB, "Adjusted", sep=".")
      
      charttitle = paste(input$tickersA, "and", sep=" ")
      charttitle = paste(charttitle, input$tickersB, sep=" ")
      charttitle = paste(charttitle, "Table Info", sep=" ")
      
      colorsA = "rgba(#R,#G,#B,1)"
      colorsA = gsub("#R", input$A_r, colorsA)
      colorsA = gsub("#G", input$A_g, colorsA)
      colorsA = gsub("#B", input$A_b, colorsA)
      
      colorsB = "rgba(#R,#G,#B,1)"
      colorsB = gsub("#R", input$B_r, colorsB)
      colorsB = gsub("#G", input$B_g, colorsB)
      colorsB = gsub("#B", input$B_b, colorsB)
      
      luminanceA = ( 0.299 * input$A_r + 0.587 * input$A_g + 0.114 * input$A_b)/255
      if (luminanceA > 0.5) {
        fontA = "rgba(0,0,0,1)"
      }
      else {
        fontA = "rgba(255,255,255,1)"
      }
      
      luminanceB = ( 0.299 * input$B_r + 0.587 * input$B_g + 0.114 * input$B_b)/255
      if (luminanceB > 0.5) {
        fontB = "rgba(0,0,0,1)"
      }
      else {
        fontB = "rgba(255,255,255,1)"
      }
      
      p = plot_ly(
        type = 'table',
        header = list(
          title=input$tickersA,
          values = c('<b>DATE</b>', paste(input$tickersA, as.character(input$dateA), sep=" "), paste(input$tickersB, as.character(input$dateB), sep=" ")),
          line = list(color = '#000000'),
          fill = list(color = c('#888888', colorsA, colorsB)),
          align = c('left','center'),
          font = list(color = c('white', fontA, fontB), size = 12)
        ),
        cells = list(
          values = rbind(
            c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted'),
            c(coredata(sym_xtsA[input$dateA,openA]), coredata(sym_xtsA[input$dateA,highA]), coredata(sym_xtsA[input$dateA,lowA]),
              coredata(sym_xtsA[input$dateA,closeA]), coredata(sym_xtsA[input$dateA,volA]), coredata(sym_xtsA[input$dateA,adjA])),
            c(coredata(sym_xtsB[input$dateB,openB]), coredata(sym_xtsB[input$dateB,highB]), coredata(sym_xtsB[input$dateB,lowB]),
              coredata(sym_xtsB[input$dateB,closeB]), coredata(sym_xtsB[input$dateB,volB]), coredata(sym_xtsB[input$dateB,adjB]))),
          line = list(color = '#000000'),
          fill = list(color = c('#AAAAAA', colorsA, colorsB)),
          align = c('left', 'center'),
          font = list(color = c('white', fontA, fontB), size = 12)
        ))%>%
        layout(
          title=charttitle)
    }
    
    else {
      
      charttitle = paste(input$tickersA, "Table Info", sep=" ")
      colorsA = "rgba(#R,#G,#B,1)"
      colorsA = gsub("#R", input$A_r, colorsA)
      colorsA = gsub("#G", input$A_g, colorsA)
      colorsA = gsub("#B", input$A_b, colorsA)
      
      luminanceA = ( 0.299 * input$A_r + 0.587 * input$A_g + 0.114 * input$A_b)/255
      if (luminanceA > 0.5) {
        fontA = "rgba(0,0,0,1)"
      }
      else {
        fontA = "rgba(255,255,255,1)"
      }
      
      p = plot_ly(
        type = 'table',
        header = list(
          title=input$tickersA,
          values = c('<b>DATE</b>', as.character(input$dateA)),
          line = list(color = '#000000'),
          fill = list(color = c('#888888', colorsA)),
          align = c('left','center'),
          font = list(color = c('white', fontA), size = 12)
        ),
        cells = list(
          values = rbind(
            c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted'),
            c(coredata(sym_xtsA[input$dateA,openA]), coredata(sym_xtsA[input$dateA,highA]), coredata(sym_xtsA[input$dateA,lowA]),
              coredata(sym_xtsA[input$dateA,closeA]), coredata(sym_xtsA[input$dateA,volA]), coredata(sym_xtsA[input$dateA,adjA]))),
          line = list(color = '#000000'),
          fill = list(color = c('#AAAAAA', colorsA)),
          align = c('left', 'center'),
          font = list(color = c('white', fontA), size = 12)
        ))%>%
        layout(
          title=charttitle)
      
    }
    
  })
  
  output$aboutme <- renderUI({
    
    HTML("<h3><strong>Created By:</strong></h3></br>
         <h4>Ian Ho - Computer Science Undergraduate from Simon Fraser University</h4></br>
         <a href='https://github.com/ho-ian'>Github</a></br>
         <a href='https://www.linkedin.com/in/ian-ho-015149106/'>LinkedIn</a></br>
         <h3><strong>Description:</strong></h3></br>
         <h4>The main idea for this little project was to be able to visualize common types of 
         data in a new way. I had always thought radar charts could show attributes of a given
         object in a special way. In order to achieve this effect, I had to normalize the data
         types against themselves to form standardized values. Therefore the points showed in 
         radar chart are their z scores. So, if a particular attribute is very high, then it was
         very high relative to itself in its whole history of ticker data. Comparing two stock tickers
         doesn't normalize both sets of data against themselves as it still remains that they 
         normalize based on their own values. However, it does show how the two stocks may
         act relatively similar or dissimilar at a given point in time. A table is offered below 
         to give real numbers.</h4></br>
         <h3><strong>Contributors:</strong></h3></br>
         </br>
         <h3><strong>Github Page:</strong></h3></br>
         <a href='https://github.com/ho-ian/HistoricalTradingData'>Link</a>")
    
  })
  
  output$resources <- renderUI({
    
    HTML("<h3><strong>Resources Used:</strong></h3></br>
         <a href='http://investexcel.net/all-yahoo-finance-stock-tickers/'>List of Stock Tickers from Yahoo's Api</a></br>")
    
  })
  
})
