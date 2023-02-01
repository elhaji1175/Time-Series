library(dplyr) # data wrangling
library(ggplot2) # visualization
library(dygraphs) # interactive time series visualization
library(tseries) # for stationarity tests
library(xts) # handling time series objects
library(forecast) # forecasting and diagnostics
library(readxl)

getwd()
setwd("/Users/lighttrading/Desktop")
tbl <- readxl::read_xlsx("SMA 443 data.xlsx")
df <- as.data.frame(tbl)
# convert the date column to date data type
df$Date = as.Date(strftime(df$Date,'%Y-%m-%d'))

# create a time series object for monthly closing prices
hsi.close <- ts(df$Close, start=c(2006,1), end=c(2020,12), frequency = 12)

# create a time series object for monthly low 
hsi.low <- ts(df$Low, start=c(2006,1), end=c(2020,12), frequency = 12)

# create a time series object for monthly high
hsi.high <- ts(df$High, start=c(2006,1), end=c(2020,12), frequency = 12)

# combine the three time series object
hsi.ts <- cbind("Close"=hsi.close,"High"=hsi.high,"Low"=hsi.low)

# Plot the time series of the high, low and closing prices for HSI share index
dygraph(hsi.ts,main = "Hang Seng Index Monthly") %>%
  dySeries('Close',color = "green",strokeWidth = 2, drawPoints = TRUE) %>%
  dyGroup(c('High','Low'),color=c("red","black"),strokePattern = c("dashed","dashed")) %>%
  dyShading(from = 'Low', to = "High", axis = "y")


# with range selector
dygraph(hsi.ts,main = "Hang Seng Index Monthly") %>%
  dyRangeSelector() %>%
  dySeries('Close',color = "green",strokeWidth = 3, drawPoints = TRUE) %>%
  dyGroup(c('High','Low'),color=c("#D8AE5A","blue"),strokeWidth=1,strokePattern = c("dotted","dashed")) %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesOpts = list(strokeWidth = 2),
              highlightSeriesBackgroundAlpha = 0.4,
              hideOnMouseOut = FALSE)










