filename_stocks <- list.files("C:/Users/Usuario/Desktop/P2/Stocks", pattern="*.us.txt", full.names=TRUE)
stock_ticker <- str_match(filename_stocks, "C:/Users/Usuario/Desktop/P2/Stocks/(.*?).us.txt")[,2]
stock_ls <- lapply(filename_stocks, fread)
names(stock_ls) <- stock_ticker
stock_dt <- rbindlist(stock_ls, idcol = "Stock_Ticker")

stock_dt[,2] <- as.Date(stock_dt$Date, "%Y-%m-%d")
stock_dt$Volume <- as.double(stock_dt$Volume)
stock_dt <- stock_dt[,-8]

aa <- "aapl"

Date1 <- as.Date("2007-01-01")
Date2 <- as.Date("2016-10-23")

AMZNData <- stock_dt[,-1] %>% filter((stock_dt$Stock_Ticker == aa) &
                                       (stock_dt$Date >= Date1 & stock_dt$Date <= Date2))

AMZNxts <- xts(x = AMZNData[,-1], order.by = AMZNData$Date)

bbData <- BBands(AMZNxts[,4], sd=2.0, n=20, maType=SMA) #Calculo de la Bollinger Bands
rsiData <- RSI(AMZNxts[,4], n=14) #Calculo del RSI
allData <- cbind(AMZNxts,bbData,rsiData) #Se quiere juntar todo en un XTS.
names(allData)
Candle <- allData[,c(1:4,6:8)]

p1 <- dygraph(Candle, main = paste("Stock: ", toupper(aa))) %>% dyCandlestick() %>%
  dySeries("dn", strokeWidth = 2, strokePattern = "dashed", color = "darkgreen") %>%
  dySeries("up", strokeWidth = 2, strokePattern = "dashed", color = "darkgreen") %>%
  dySeries("mavg", strokeWidth = 1.5, color = "red") 
p1

dailyROR <- dailyReturn(Candle$Close, type = "log")
names(dailyROR) <- "MA.Log.Returns"
# Plot the log-returns    
dailyROR %>%    
  ggplot(aes(x = MA.Log.Returns)) + 
  geom_histogram(bins = 100) + 
  geom_density() +
  geom_rug(alpha = 0.5)

probs <- c(.005, .025, .25, .5, .75, .975, .995)
dist_log_returns <- dailyROR %>% 
  quantile(probs = probs, na.rm = TRUE)

mean_log_returns <- mean(dailyROR, na.rm = TRUE)
sd_log_returns <- sd(dailyROR, na.rm = TRUE)
summary1<- data.frame(c("mean","sd"), c(mean_log_returns,sd_log_returns))
summary1

N     <- 252 # Number of Stock Price Simulations
M     <- 250  # Number of Monte Carlo Simulations   
mu    <- mean_log_returns
sigma <- sd_log_returns
day <- 1:N
price_init <- Candle$Close[[nrow(Candle$Close)]]
# Simulate prices
set.seed(123)
monte_carlo_mat <- matrix(nrow = N, ncol = M)
for (j in 1:M) {
  monte_carlo_mat[[1, j]] <- price_init
  for(i in 2:N) {
    monte_carlo_mat[[i, j]] <- monte_carlo_mat[[i - 1, j]] * exp(rnorm(1, mu, sigma))
  }
}
# Format and organize data frame
price_sim <- cbind(day, monte_carlo_mat) %>%
  as_tibble() 
nm <- str_c("Sim.", seq(1, M))
nm <- c("Day", nm)
names(price_sim) <- nm
price_sim <- price_sim %>%
  gather(key = "Simulation", value = "Stock.Price", -(Day))
# Visualize simulation
price_sim %>%
  ggplot(aes(x = Day, y = Stock.Price, Group = Simulation)) + 
  geom_line(alpha = 0.1) +
  ggtitle(str_c("AAPL: ", M, 
                " Monte Carlo Simulations for Prices Over ", N, 
                " Trading Days"))

end_stock_prices <- price_sim %>% 
  filter(Day == max(Day))
probs <- c(.005, .025, .25, .5, .75, .975, .995)
dist_end_stock_prices <- quantile(end_stock_prices$Stock.Price, probs = probs)
dist_end_stock_prices %>% round(2)

# 
# What can you say about the relationship between the standard deviation and mean of the log returns?
# Does there appear to be one? 
# As volatility (standard deviation of returns) increases, what tends to happen to growth (mean increase of returns)?

AMZNData <- stock_dt[,-1] %>% filter((stock_dt$Stock_Ticker == "amzn") &
                                       (stock_dt$Date >= Date1 & stock_dt$Date <= Date2))
names(AMZNData)[names(AMZNData) == "Close"] <- "AMZN"
MSFTData <- stock_dt[,-1] %>% filter((stock_dt$Stock_Ticker == "ndaq") &
                                       (stock_dt$Date >= Date1 & stock_dt$Date <= Date2))
names(MSFTData)[names(MSFTData) == "Close"] <- "ndaq"
AAPLData <- stock_dt[,-1] %>% filter((stock_dt$Stock_Ticker == "aapl") &
                                       (stock_dt$Date >= Date1 & stock_dt$Date <= Date2))
names(AAPLData)[names(AAPLData) == "Close"] <- "AAPL"
NFLXData <- stock_dt[,-1] %>% filter((stock_dt$Stock_Ticker == "nflx") &
                                       (stock_dt$Date >= Date1 & stock_dt$Date <= Date2))
names(NFLXData)[names(NFLXData) == "Close"] <- "NFLX"
XOMData <- stock_dt[,-1] %>% filter((stock_dt$Stock_Ticker == "xom") &
                                      (stock_dt$Date >= Date1 & stock_dt$Date <= Date2))
names(XOMData)[names(XOMData) == "Close"] <- "XOM"

data1 <- data.frame(AMZNData$Date,AMZNData$AMZN.Close, MSFTData$Close, AAPLData$Close)
CorStocks <- data.frame(MSFTData$Date,AMZNData$AMZN, MSFTData$MSFT, AAPLData$AAPL, NFLXData$NFLX, XOMData$XOM)
names(CorStocks)<- c("Date","AMZN","MSFT","AAPL","NFLX","XOM")

correlation <- CorStocks %>% select(-Date) %>% cor()

correlation %>%  corrplot(order   = "hclust", addrect = 11)
AAPLData$Color <- AAPLData$Close - AAPLData$Open 

f1 <- ggplot(AAPLData, aes(Date,Close)) + scale_colour_identity() + geom_line(color = "blue") +
  ggtitle("AAPL") +
  theme_minimal()
f1

f2 <- ggplot(AAPLData) +
  geom_bar(stat="identity", aes(Date, Volume, fill = ifelse(as.numeric(Color) < 0, "darkred", "darkgreen")),
           show.legend = FALSE) +
  theme_minimal()
f <- grid.arrange(f1, f2, ncol = 1, heights = c(3, 1.5))
