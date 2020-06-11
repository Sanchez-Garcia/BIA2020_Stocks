#### PASOS PREVIOS####
#Instalacion de paquetes y librerias necesarias.
# install.packages('bit64')
install.packages("shiny")
if(!require("tidyverse")) {
  install.packages("tidyverse", repos="https://cloud.r-project.org/",
                   quiet=TRUE, type="binary")
  library("tidyverse")
}
install.packages("gridExtra")
# install.packages("plotly")
install.packages("dplyr")

#Llamada a librerias
library(stringr)
library(data.table)
library(shiny)
library(shinydashboard)
library(dplyr)
library(gridExtra)
library(plotly)
library(quantmod)
library(TTR)
library(dygraphs)

#### LECTURA DE DATOS EXTERNOS ####
filename_stocks <- list.files("C:/Users/Usuario/Desktop/P2/Stocks", pattern="*.us.txt", full.names=TRUE)
stock_ticker <- str_match(filename_stocks, "C:/Users/Usuario/Desktop/P2/Stocks/(.*?).us.txt")[,2]
stock_ls <- lapply(filename_stocks, fread)
names(stock_ls) <- stock_ticker
stock_dt <- rbindlist(stock_ls, idcol = "Stock_Ticker")

stock_dt[,2] <- as.Date(stock_dt$Date, "%Y-%m-%d")
stock_dt$Volume <- as.double(stock_dt$Volume)
stock_dt <- stock_dt[,-8]
remove(stock_ls)

# Indicator <- c("ROR", "BB Banwidth")
# Value <- c(NA, NA)
# Indicators_df <- data.frame(Indicator, Value)
# 
# #### A?ADIMOS ALGUNOS DATOS IMPORTANTES ####
# stock_dt$chg <- ifelse(stock_dt$Close - stock_dt$Open > 0, "up", "dn")
# Graph_width <- 0.85

# stock_dt$TP <- (stock_dt$High + stock_dt$Low + stock_dt$Close)/3

# filename_ETF <- list.files("C:/Users/Usuario/Desktop/P2/ETFs", pattern="*.us.txt", full.names=TRUE)
# ETF_ticker <- str_match(filename_ETF, "C:/Users/Usuario/Desktop/P2/ETFs/(.*?).us.txt")[,2]
# etf_ls <- lapply(filename_ETF, fread)
# names(etf_ls) <- ETF_ticker
# etf_dt <- rbindlist(etf_ls, idcol = "ETF_Ticker")

#### CREACION DE SHINY APP####
ui <- dashboardPage(
                     dashboardHeader(title = "Stock Analysis"),
                     dashboardSidebar(disable = TRUE),
                     dashboardBody(
  fluidRow(
    box(
      title = "Enter Stock Code", width = 2, solidHeader = TRUE, status = "primary",
      selectInput("StockName", "Stock", stock_ticker, width = "100%"),
      dateRangeInput("DateRange", label= "Date Range:", start ="2005-01-01", end = "2015-01-01"),
      actionButton("Start", "Go")
    ),
    box(
      title = "Graph Adjustment", width = 10, solidHeader = TRUE, status = "primary",
      numericInput("Periods", "Number of Periods (in days): ", 20, min = 1, max = 100),
      radioButtons(inputId = "Options", label = "Graph Type:",
                   choices = c("Close with Volume" = "Line_Volume",
                               "Candle Chart" = "Candle",
                               "Log Returns Normal" = "LogReturns",
                               "Monte Carlo" = "Monte"), 
                   selected = "Candle")
      # Poner checkBoxes y demás para customizar el gráfico.
    )
    
  ),
  fluidRow(
    box(
      title = "Indicators", width = 2, solidHeader = TRUE, status = "primary",
      tableOutput(outputId = "Indicators")
    ),
    box(
      title = "Charts", solidHeader = TRUE, status = "primary", width = 10,
      dygraphOutput(outputId = "History", width ="100%"),
    )
  )
  )
)

#### SERVIDOR APP ####
server <- shinyServer (function(input, output, session) {
  filterDat_dt <- reactive({
    stock_dt[,-1] %>% filter((stock_dt$Stock_Ticker == as.character(input$StockName)) &
                          (stock_dt$Date >= min(input$DateRange) & stock_dt$Date <= max(input$DateRange)))
  })
  
  filterXTS <- reactive({xts(x = filterDat_dt()[,-1], order.by = filterDat_dt()$Date)}) #Se pasa a XTS.
  bbData <- reactive({BBands(filterXTS()[,4], sd=2.0, n=input$Periods, maType=SMA)}) #Calculo de la Bollinger Bands
  rsiData <- reactive({RSI(filterXTS()[,4], n=14)}) #Calculo del RSI
  
  allData <- reactive({cbind(filterXTS(),bbData(),rsiData())}) #Se quiere juntar todo en un XTS.
  
  ROR <- reactive({(filterXTS()$Close[length(filterXTS()$Close)]-filterXTS()$Close[1])/filterXTS()$Close[1]}) #Claculo retorno periodo.
  BBBW <- reactive({bbData()$up - bbData()$dn}) #Calculo de indicador de riesgo BBBW.
  
  # Calcular retornos y ver si tienen una funcionalidad distribucion normal.
  Candle <- reactive({allData()[,c(1:4,6:8)]})
  dailyROR <- reactive({dailyReturn(Candle()$Close, type = "log")})
  
  drawChart <- eventReactive(input$Start,{
    if(input$Options == "Line_Volume"){
      dataset <- reactive({allData()[,c(1:5)]})
      
      output$History <- renderDygraph({
        dygraph(dataset()[,4])
      })
      
    }else if(input$Options == "Candle"){
      Candle <- reactive({allData()[,c(1:4,6:8)]})
      output$History <- renderDygraph({
      p1 <- dygraph(Candle(), main = paste("Stock: ", toupper(input$StockName))) %>% dyCandlestick() %>%
        dySeries("dn", strokeWidth = 2, strokePattern = "dashed", color = "darkgreen") %>%
        dySeries("up", strokeWidth = 2, strokePattern = "dashed", color = "darkgreen") %>%
        dySeries("mavg", strokeWidth = 1.5, color = "red") %>%
        dyRangeSelector()
      })
    }else if(input$Options == "LogReturns"){
      Candle <- reactive({allData()[,c(1:4,6:8)]})
      dailyROR <- reactive({dailyReturn(Candle()$Close, type = "log")})
      output$History <- renderPlot({
        dailyROR() %>%
          ggplot(aes(x = daily.returns)) +
          geom_histogram(bins = 100) +
          geom_density() +
          title("Normality of Log Returns") +
          geom_rug(alpha = 0.5)
      })
    }else if(input$Options == "Monte"){
      Candle <- reactive({allData()[,c(1:4,6:8)]})
      mean_log_returns <- reactive({mean(dailyROR, na.rm = TRUE)})
      sd_log_returns <- reactive({sd(dailyROR, na.rm = TRUE)})

      N     <- 252 # Number of Stock Price Simulations (Will be an input)
      M     <- 250  # Number of Monte Carlo Simulations (Will be an input)
      mu    <- reactive({mean_log_returns})
      sigma <- reactive({sd_log_returns})
      day <- reactive({1:N})
      price_init <- reactive({Candle()$Close[[nrow(Candle()$Close)]]})
      # Simulate prices
      set.seed(123)
      monte_carlo_mat <- reactive({matrix(nrow = N, ncol = M)})
      for (jj in 1:M) {
        monte_carlo_mat()[[1, jj]] <- price_init
        for(ii in 2:N) {
          monte_carlo_mat()[[ii, jj]] <- monte_carlo_mat()[[ii - 1, jj]] * exp(rnorm(1, mu(), sigma()))
        }
      }
      # Format and organize data frame
      price_sim <- reactive({cbind(day, monte_carlo_mat) %>%
        as_tibble()
        })
      nm <- reactive({str_c("Sim.", seq(1, M))})
      nm <- reactive({c("Day", nm)})
      # names(price_sim) <- nm
      price_sim <- reactive({price_sim %>%
        gather(key = "Simulation", value = "Stock.Price", -(Day))
        })
      # Visualize simulation
      output$History <- renderPlot({
      price_sim %>%
        ggplot(aes(x = Day, y = Stock.Price, Group = Simulation)) +
        geom_line(alpha = 0.1) +
        ggtitle(paste(input$StockName,": ", M,
                      " Monte Carlo Simulations for Prices Over ", N,
                      " Trading Days"))
      })
    }
    })

  output$Indicators <- renderTable(
   ROR()
  )
  #Condicionales para agrupar datos de distintas formas.
 
  # output$LogReturns <- renderPlot({
  #   dailyROR() %>%
  #     ggplot(aes(x = daily.returns)) +
  #     geom_histogram(bins = 100) +
  #     geom_density() +
  #     title("Normality of Log Returns") +
  #     geom_rug(alpha = 0.5)
  # 
  # })
  # output$History <- renderDygraph({
  #   p1 <- dygraph(Candle(), main = paste("Stock: ", toupper(input$StockName))) %>% dyCandlestick() %>%
  #     dySeries("dn", strokeWidth = 2, strokePattern = "dashed", color = "darkgreen") %>%
  #     dySeries("up", strokeWidth = 2, strokePattern = "dashed", color = "darkgreen") %>%
  #     dySeries("mavg", strokeWidth = 1.5, color = "red") %>%
  #     dyRangeSelector()
  # })
})

  #### RUN APP ####
shinyApp(ui = ui, server = server)

