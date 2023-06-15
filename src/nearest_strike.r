prices <- dx_ticker
names(prices)[1:2] <- c("symbol", "current_price")

strike_prices <- dx_tkr_stk[EXPDAY %like% '2023-01-20', c(1:3)]
names(strike_prices)[1:3] <- c("symbol", "option_type", "strike_price")

strike_prices <- strike_prices[prices, on = .(symbol)]

# Compute the absolute difference between strike and current prices
strike_prices[, abs_diff := abs(strike_price - current_price)]

# Find the nearest strike price for each symbol
nearest_strikes <- strike_prices[, .(nearest_strike = strike_price[which.min(abs_diff)]), by = .(symbol, option_type)]
nearest_strikes <-  nearest_strikes[prices, on = .(symbol)][,c(1:2,4:3)]
names(nearest_strikes)[4] <- "strike_price"

# Join all strike prices for each symbol
joined_strikes <- nearest_strikes[strike_prices, on = .(symbol, option_type)][,-6]
names(joined_strikes)[5] <- "nearest_strike"

# Filter the strike prices above and below with at least $5.00 difference
strike_prices_above <- joined_strikes[strike_price > nearest_strike & strike_price <= nearest_strike + 2 * 5]
strike_prices_below <- joined_strikes[strike_price < nearest_strike & strike_price >= nearest_strike - 2 * 5]

# Remove unnecessary columns
strike_prices_above[, `:=`(nearest_strike = NULL, abs_diff = NULL)]
strike_prices_below[, `:=`(nearest_strike = NULL, abs_diff = NULL)]

dx_strike_prices <- setorder(rbind(strike_prices_below, nearest_strikes, strike_prices_above), symbol, option_type, strike_price)
dx_strike_prices[nearest_strikes, on =.(symbol, option_type)]

# Merge the results into a single data.table
results <- cbind(nearest_strikes, strike_above = strike_prices_above, strike_below = strike_prices_below)

results[, position := ifelse(strike_price == nearest_strike, 0,
                             ifelse(strike_price > nearest_strike, rank(-strike_price, ties.method = "first") - 1,
                                    -rank(strike_price, ties.method = "first") + 1))]



library(data.table)

# Create two data.tables
dt1 <- data.table(key = c(1, 2, 3, 4, 5), value1 = letters[1:5])
dt2 <- data.table(key = c(1, 2, 3, 4, 6), value2 = LETTERS[1:5])

# Define the condition for the join
condition <- function(key_value) {
  if (key_value %% 2 == 1) {
    return("odd_join")
  } else {
    return("even_join")
  }
}

# Perform the join based on the condition
if (condition(1) == "odd_join") {
  result <- dt1[dt2, on = "key", nomatch = 0]
} else {
  result <- dt1[dt2, on = "key", nomatch = 0]
}

# Print the result
print(result)
