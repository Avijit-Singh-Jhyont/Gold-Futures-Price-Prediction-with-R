#Installing necessary packages if not already installed
if(!require(quantmod)) install.packages("quantmod")
if(!require(forecast)) install.packages("forecast")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(lubridate)) install.packages("lubridate")

#Necessary Libraries
library(quantmod)
library(forecast)
library(ggplot2)
library(lubridate)


#Fetching historical Gold Futures data from Yahoo Finance; assigned to data frame `GC=F`
getSymbols("GC=F",src="yahoo",from="2010-01-01",to=Sys.Date(),auto.assign = TRUE)

#Extracting Adjusted Prices in form of 'GC=F.Adjusted' col from GC-F
gold_prices <- `GC=F`[,"GC=F.Adjusted"]

#To fill in any missing data in the source dataset from yahoo Finance
gold_prices <-  na.approx(gold_prices)

#Renaming col for easier reference
colnames(gold_prices) <- "Adjusted"

#converting gold_prices to a time Series object for analysis
gold_ts <- ts(gold_prices, start=c(year(index(gold_prices)[1]), month(index(gold_prices)[1])), frequency = 365)

# Plotting Adjusted close prices over time
ggplot(data=as.data.frame(gold_ts), aes(x=index(gold_prices),y=Adjusted))+
  geom_line(color="blue") +
  labs(title="Historical Gold Prices", x ="Date", y="Adjusted close Price")+theme_minimal()

#Fitting an ARIMA model to the time series data
fit <- auto.arima(gold_ts)
summary(fit)

#Forecasting the next 180 days(6 mo)
forecasted_values <- forecast(fit, h=180)
plot(forecasted_values)

#Checking residuals to evaluate the degree of fit of the project
checkresiduals(fit)



