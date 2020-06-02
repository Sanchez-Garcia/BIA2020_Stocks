#### PASOS PREVIOS####
#Instalacion de paquetes y librerias necesarias.
install.packages('bit64')
install.packages("shiny")
if(!require("tidyverse")) {
  install.packages("tidyverse", repos="https://cloud.r-project.org/",
                   quiet=TRUE, type="binary")
  library("tidyverse")
}
install.packages("gridExtra")
# install.packages("plotly")

#Llamada a librerias
library(stringr)
library(data.table)
library(shiny)
library(shinydashboard)
library(dplyr)
library(gridExtra)
library(plotly)
# library(quantmod)
# library(TTR)

#### LECTURA DE DATOS EXTERNOS ####
filename_stocks <- list.files("C:/Users/Usuario/Desktop/P2/Stocks", pattern="*.us.txt", full.names=TRUE)
stock_ticker <- str_match(filename_stocks, "C:/Users/Usuario/Desktop/P2/Stocks/(.*?).us.txt")[,2]
stock_ls <- lapply(filename_stocks, fread)
names(stock_ls) <- stock_ticker
stock_dt <- rbindlist(stock_ls, idcol = "Stock_Ticker")
stock_dt[,2] <- as.Date(stock_dt$Date, "%Y-%m-%d")
stock_dt$Volume <- as.double(stock_dt$Volume)
stock_dt <- stock_dt[,-8]

#### AÑADIMOS ALGUNOS DATOS IMPORTANTES ####
stock_dt$Color <- stock_dt$Close - stock_dt$Open
stock_dt$TP <- (stock_dt$High + stock_dt$Low + stock_dt$Close)/3

# filename_ETF <- list.files("C:/Users/Usuario/Desktop/P2/ETFs", pattern="*.us.txt", full.names=TRUE)
# ETF_ticker <- str_match(filename_ETF, "C:/Users/Usuario/Desktop/P2/ETFs/(.*?).us.txt")[,2]
# etf_ls <- lapply(filename_ETF, fread)
# names(etf_ls) <- ETF_ticker
# etf_dt <- rbindlist(etf_ls, idcol = "ETF_Ticker")


# head(etf_dt)

#....#
# getSymbols("AAPL")
# chartSeries(AAPL,subset="last 3 months")

# head(AAPL)
#....#

#### CREACION DE SHINY APP####
# Define UI for app that draws a histogram ----

ui <- dashboardPage(
                     dashboardHeader(title = "Stock Analysis"),
                     dashboardSidebar(disable = TRUE),
                     dashboardBody(
  fluidRow(
    box(
      title = "Enter Stock Code", width = 4, solidHeader = TRUE, status = "primary",
      selectInput("StockName", "Stock", stock_ticker, width = "100%"),
      dateRangeInput("DateRange", label= "Date Range:", start ="2005-01-01", end = "2015-01-01")
  ),
    valueBoxOutput("value1"),
    valueBoxOutput("value2"),
    valueBoxOutput("value3"),
    valueBoxOutput("value4")
  ),
  box(
    title = "Charts", width = "100%", solidHeader = TRUE, status = "primary",
    plotOutput(outputId = "History", width ="100%")
  )
  )
)

#### SERVIDOR APP ####
#Define server logic required to draw a histogram ----
server <- shinyServer(function(input, output, session) {
  filterDat_dt <- reactive({
    stock_dt %>% filter((stock_dt$Stock_Ticker == as.character(input$StockName)) &
                          (stock_dt$Date >= min(input$DateRange) & stock_dt$Date <= max(input$DateRange)))
    })
  # filterDat_dt$MB <- SMA(filterDat_dt$TP, n=20)
  # filterDat_dt$UB <- filterDat_dt$MB + 2 * sd(filterDat_dt$TP)
  # filterDat_dt$LB <- filterDat_dt$MB - 2 * sd(filterDat_dt$TP)
  
  output$value1 <- renderValueBox({
    valueBox(subtitle = "BB BanWidth",
      mean(4*sd(filterDat_dt$TP)),
      icon = icon("risk"),
      color = "blue"
    )
  })
  output$value2 <- renderValueBox({
    valueBox(subtitle = "KPI 2",
      sample(1:10, 1),
      icon = icon("server"),
      color = "orange"
    )
  })
  output$value3 <- renderValueBox({
    valueBox(subtitle = "KPI 3",
      sample(1:10, 1),
      icon = icon("cookie"),
      color  = "red"
    )
  })
  output$value4 <- renderValueBox({
    valueBox(subtitle = "KPI 4",
             sample(1:10, 1),
             icon = icon("menu-hamburger", lib = "glyphicon"),
             color = "green"
    )
  })
  
  output$History <- renderPlot({
    f1 <- ggplot(filterDat_dt(), aes(Date,Close)) + scale_colour_identity() + geom_area(color = "black",
                                                                                   fill = "light grey" ) +
      theme_minimal()

    f2 <- ggplot(filterDat_dt()) +
      geom_bar(stat="identity", aes(Date, Volume, fill = ifelse(as.numeric(Color) < 0, "#CA0020", "#4DAC26")),
               show.legend = FALSE) +
      theme_minimal()
    f <- grid.arrange(f1, f2, ncol = 1, heights = c(3, 1.5))
    })
})

  #### RUN APP ####
shinyApp(ui = ui, server = server)

output$value3 <- renderValueBox({
  valueBox(
    formatC(prof.prod$value, format="d", big.mark=',')
    ,paste('Top Product:',prof.prod$Product)
    ,icon = icon("menu-hamburger",lib='glyphicon')
    ,color = "yellow")   
})