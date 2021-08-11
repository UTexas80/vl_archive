# ------------------------------------------------------------------------------
# 01. How to create a stock screener           https://tinyurl.com/64dpjxh9  ---
# ------------------------------------------------------------------------------
################################################################################
## 01. collecting price data                                                 ###
################################################################################
# Load libraries
library(data.table)
library(magrittr)
library(quantmod)
# ------------------------------------------------------------------------------
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# ------------------------------------------------------------------------------
# Read data from ishares site to get stock tickers
# ------------------------------------------------------------------------------
ticker_lkp = suppressWarnings(fread("https://www.ishares.com/uk/individual/en/products/253741/ishares-nasdaq-100-ucits-etf/1506575576011.ajax?fileType=csv&fileName=CNDX_holdings&dataType=fund", showProgress=F))
# ------------------------------------------------------------------------------
# get stock tickers
tickers = ticker_lkp[nchar(ISIN) == 12, `Issuer Ticker`]
# ------------------------------------------------------------------------------
# Get prices as xts
data = list()
for (i in seq_along(tickers)) {
  Sys.sleep(0.005) # not to get rate limited
  ticker = tickers[i]
  from = "2015-01-01"
  to = as.character(Sys.Date())
  data[[ticker]] = try(getSymbols(ticker, from=from, to=to, auto.assign = FALSE, src='yahoo'), silent=TRUE)
}
names(data) = tickers
# ------------------------------------------------------------------------------
# Remove empty data
table(sapply(data, function(x) class(x)[1]))

tail(data[['MSFT']], 3)
tail(data[['COST']], 3)
################################################################################
## 02. develop the screener                                                 ###
################################################################################

# ------------------------------------------------------------------------------
# 02.01 adjust the OHLC values.
# ------------------------------------------------------------------------------
data = lapply(data, adjustOHLC, use.Adjusted=TRUE)
data = lapply(data, function(d) d['/20210115'])                                # today's data
# ------------------------------------------------------------------------------
# Screener parameters
# Long term uptrend: Close > SMA-150 (long term uptrend)
# High volatility: ATR t10 > 4% (high volatility stocks)
# Short term oversold: RSI t3 < 30
# Ranked by lower RSI-3
# ------------------------------------------------------------------------------
# Definitions:
#   ATR: Average true range
#   RSI: Relative strenght indicator
#   SMA-150: Simple moving average of the past 150 days
# ------------------------------------------------------------------------------
# mean_reversion function
# ------------------------------------------------------------------------------
mean_reversion <- function(data, ticker){

  # Read the data
  df = data[[ticker]]

  # Compute indicators
  HLC = HLC(df)
  sma150 = SMA(Cl(df), n=150)
  atr10 = ATR(HLC, n=10, maType='EMA')
  rsi3 = RSI(Cl(df), n=3)

  # join them to the main data
  df = merge(df, sma150)
  df = merge(df, atr10$atr)
  df = merge(df, rsi3)
  df$atr = df$atr / Cl(df)
  df$close = Cl(df)

  # Select relevant columns
  keep_cols = c("SMA", "atr", "rsi", "close")
  df = df[, keep_cols]
  names(df)[1:3] = c("sma150", "atr10", "rsi3")

  # Get the most recent observation
  df = tail(df, 1)

  # Convert the output as a data table
  dt = as.data.table(df)
  dt[, ticker:=ticker]
  setnames(dt, "index", "date")

  dt
}
# ------------------------------------------------------------------------------
# 02.02 Execute the screener and evaluate results
# ------------------------------------------------------------------------------
ndicators = list()
for(ticker in tickers){
  indicators[[ticker]] = mean_reversion(data, ticker)
}

# Rbind the indicators list
indicators = rbindlist(indicators)
head(indicators)
# ------------------------------------------------------------------------------
# 02.03 Keep stocks with close higher than the SMA-150
# ------------------------------------------------------------------------------

# 1. Long-Term Trend
indicators = indicators[close >= sma150]
nrow(indicators)

# 2. ATR-10 > 4%
indicators = indicators[atr10 >= 0.04] 
nrow(indicators)

# 3. RSI-3 < 30
indicators = indicators[rsi3 <= 30]
nrow(indicators)

# 4. Rank by RSI-3
indicators[, rk:=frank(rsi3)]
indicators[order(rk)]

# ------------------------------------------------------------------------------
# 03.00 Example plot of one stock that was ranked in the top-10
# ------------------------------------------------------------------------------
df = data[['PDD']]["2020/"]
{plot(Cl(df))
lines(SMA(Cl(df), n = 20), col="blue")
lines(SMA(Cl(df), n = 50), col="red", lty=2)
# add legend to panel 1
addLegend("topleft", legend.names = c("Close", "SMA(20)", "SMA(50)"), 
          lty=c(1, 1, 2), lwd=c(2, 1, 1),
          col=c("black", "blue", "red"))}