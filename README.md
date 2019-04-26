# Historical Trading Data
I wanted to create an app that could visualize stock ticker data in a different way. I thought that a radar chart was both visually
appealing as well as informative in a different way; being able to compare multiple attributes at once. I used one of R's many 
packages that could be used for obtaining and analyzing stock data (quantmod). I could gather stock ticker data from yahoo's api
as long as I knew the name. I used a list of ticker names from an excel spreadsheet found online. 

* [Deployed Version (may take a long time to load)](https://hoianshiny.shinyapps.io/HistoricalTradingData/)

## Getting Started
To get started using my app locally, just pull the repository and make sure you have R installed with shiny web app capabilities.

### Prerequisites
The only required libraries are 'quantmod', 'plotly', and 'xts'.

Run these commands in the R console inside RSTUDIO:
install.packages('quantmod')
install.packages('plotly')
install.packages('xts')

### Running the App
Once everything is setup, click 'Run App' or use the command runApp('HistoricalTradingData') in the console.

### Examples
![Single Chart](https://github.com/ho-ian/HistoricalTradingData/blob/master/screenshot/1.png)
![Comparison Chart](https://github.com/ho-ian/HistoricalTradingData/blob/master/screenshot/2.png)

## Built With
* [List of Yahoo Ticker Names](http://investexcel.net/all-yahoo-finance-stock-tickers/)
