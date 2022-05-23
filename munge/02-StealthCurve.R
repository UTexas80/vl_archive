# https://tinyurl.com/jw5bcphc
# library(Rcharts)

# Original data source - https://www.nasdaq.com/market-activity/funds-and-etfs/wmt/historical

# Step One:
# Download reformatted data (columns/headings) from github and save to a local drive
# https://github.com/123blee/Stealth_Curves.io/blob/main/WMT_nasdaq_com_data_reformatted.xlsx


# wmt <- read_excel("/Users/gfalk/Dropbox/__Blogs-and-Posts/__r_bloggers/WMT_nasdaq_com_data_reformatted.xlsx")
wmt <- WMT_nasdaq_com_data_reformatted.Sheet1
wmt

# Convert 'Date and Time' to 'Date' column
wmt[["Date"]] <- as.Date(wmt[["Date"]])
wmt

bars <- nrow(wmt)

# Add bar indicator as first tibble column

wmt <- wmt %>%
  add_column(t = 1:nrow(wmt), .before = "Date")
wmt

# Step Two - View the raw data in an interactve chart

# Interactive Pricing Chart (Low prices)

xmin <- 1              
ymin <- 0   
ymax_low <- ceiling(max(wmt[["Low"]]))

interactive_low <- rCharts::hPlot(x = "t", y = "Low", data = wmt, type = "line",
                     
                     ylim = c(ymin, ymax_low),
                     
                     xlim = c(xmin, bars),
                     
                     xaxt = "n",   # suppress x-axis labels
                     
                     yaxt = "n",   # suppress y-axis labels,
                     
                     ann = FALSE)  # x and y axis titles

interactive_low$set(height = 600)

interactive_low$set(width = 700)

interactive_low$plotOptions(line = list(color = "green"))

interactive_low$chart(zoomType = "x")   # Highlight range of chart to zoom in

interactive_low

# Step Three:
# Add 400 future days to the tibble for projection of the Stealth Curve once added

future <- 400

wmt <- wmt %>%
  add_row(t = (bars+1):(bars+future))

# Step Four: Chart the WMT daily low prices with 400 days of padding.
# Market Pivot Lows using 'Low' Prices
# Chart 'Low' WMT prices

plot.new()

background <- c("azure1")

chart_title_low <- c("Walmart (WMT) \nDaily Low Prices ($)")

u <- par("usr") 

rect(u[1], u[3], u[2], u[4], col = background) 

par(ann = TRUE)

par(new = TRUE)

t <- wmt[["t"]]
Price <- wmt[["Low"]]

plot(x=t, y=Price, main = chart_title_low, type="l", col = "blue", 
     
     ylim = c(ymin, ymax_low) ,
     
     xlim = c(xmin, (bars+future )) )    

# Step Five: Add this Stealth Support Curve to the tibble.
# Stealth Support Curve parameters
a <- -432687.30  
b <-     -57.52 
c <-   -7794.36  


wmt <- wmt %>%
  mutate(Stealth_Curve_Low = a/(t + c) + b)

# Step Six:Add NA padding where the curve does not apply.
# Omit Stealth Support Curve values from charting 

wmt[["Stealth_Curve_Low"]][1:3900] <- NA

# Step Seven: Add the calculated Stealth Support Curve to a zoomed chart.
# Chart Low wmt prices

plot.new()

u <- par("usr") 

rect(u[1], u[3], u[2], u[4], col = background) 

par(ann=TRUE)

par(new=TRUE)

chart_title_low <- c("Walmart (WMT) \nDaily Low Prices ($)")

t <- wmt[["t"]]
Price <- wmt[["Low"]]

# Focus only on the last 7 years 
xmin = 3700

plot(x=t, y=Price, main = chart_title_low, type="l", col = "blue", 
     
     ylim = c(ymin, ymax_low + 100) ,
     
     xlim = c(xmin, (bars+future )) )  


# Add Stealth Support Curve to chart

lines(t, wmt[["Stealth_Curve_Low"]])

# Step Eight: add the Stealth Resistance Curve to the tibble. The Stealth Resistance Curve
# Stealth Resistance Curve parameters

a <-  -102950.40  
b <-       47.63  
c <-    -6252.63  

# Add Stealth Curve to tibble

wmt <- wmt %>%
  mutate(Stealth_Curve_High = a/(t + c) + b)
wmt


# Omit Stealth Curve values from charting

wmt[["Stealth_Curve_High"]][1:3700] <- NA
wmt


# Add Stealth Resistance Curve to chart

lines(t, wmt[["Stealth_Curve_High"]])