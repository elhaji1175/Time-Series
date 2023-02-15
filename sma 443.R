library(dplyr) # data wrangling
library(ggplot2) # visualization
library(dygraphs) # interactive time series visualization
library(tseries) # for stationarity tests
library(xts) # handling time series objects
library(forecast) # forecasting and diagnostics
library(readxl)
library(jsonlite)
getwd()
setwd("/Users/lighttrading/Desktop")
tbl <- readxl::read_xlsx("443 dataset.xlsx")
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
  dyGroup(c('High','Low'),color=c("red","pink"),strokePattern = c("dashed","dashed")) %>%
  dyShading(from = 'Low', to = "High", axis = "y")
# with range selector
dygraph(hsi.ts,main = "Hang Seng Index Monthly") %>%
  dyRangeSelector() %>%
  dySeries('Close',color = "red",strokeWidth = 3, drawPoints = TRUE) %>%
  dyGroup(c('High','Low'),color=c("#D8AE5A","blue"),strokeWidth=1,strokePattern = c("dotted","dashed")) %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesOpts = list(strokeWidth = 2),
              highlightSeriesBackgroundAlpha = 0.4,
              hideOnMouseOut = FALSE)

# show a ribbon from low to high
dygraph(hsi.ts,main = "Hang Seng Index Monthly") %>%
  dySeries(c("Low", "Close", "High"), label = "Price") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))
# plot the time series chart for natural log of closing price
hsi.close.log <- log(hsi.close)
dygraph(hsi.close.log, main="HSI Natural Log of Closing Prices") %>% 
  dySeries('V1',strokeWidth = 2, color="red")

pacf(hsi.close.log, main="PACF plot Natural log of HSI Close")

adf.test(hsi.close.log, k=1)
kpss.test(hsi.close.log, "Level")
# automatic model
auto_model <- auto.arima(hsi.close)
auto_model

# custom model
d <- ndiffs(hsi.close)
custom_model <- Arima(hsi.close, c(1,d,1))
custom_model

checkresiduals(auto_model)

checkresiduals(custom_model)
predictions <- forecast(custom_model)


# use dygraphs to visualize predictions 95% interval

gen_array <- function(forecast_obj){
  
  actuals <- forecast_obj$x
  lower <- forecast_obj$lower[,2]
  upper <- forecast_obj$upper[,2]
  point_forecast <- forecast_obj$mean
  
  cbind(actuals, lower, upper, point_forecast)
}

ts_array <- gen_array(predictions)
graph_title <- "HSI Forecasts and 95% Confidence Intervals"
dygraph(ts_array, main = graph_title) %>% 
  dyRangeSelector() %>% 
  dyRangeSelector(height = 40,
                  dateWindow = c("2013-01-01", "2021-12-31")) %>%
  dySeries(name = "actuals", label = "actual",strokeWidth = 2, color="blue") %>%
  dySeries(c("lower","point_forecast","upper"), label = "Predicted") %>%
  dyAxis("y", label = "Closing Price") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesOpts = list(strokeWidth = 2)) %>%
  dyOptions(axisLineColor = "green", gridLineColor = "grey")







