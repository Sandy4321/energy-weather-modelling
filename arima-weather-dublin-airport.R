# Directories 
mydata<- read.csv("mly532_condensed.csv")
attach(mydata)
summary(mydata)

weatherarima <- ts(mydata$meant[1:732], start = c(1941,11), frequency = 12)
plot(weatherarima,type="l",ylab="Temperature")
title("Mean Temperature - Dublin Airport")

stl_weather = stl(weatherarima, "periodic")
seasonal_stl_weather   <- stl_weather$time.series[,1]
trend_stl_weather     <- stl_weather$time.series[,2]
random_stl_weather  <- stl_weather$time.series[,3]

plot(as.ts(seasonal_stl_weather))
title("Seasonal")
plot(trend_stl_weather)
title("Trend")
plot(random_stl_weather)
title("Random")

# Load libraries
library(MASS)
library(tseries)
library(forecast)

# ACF, PACF and Dickey-Fuller Test
acf(weatherarima, lag.max=20)
pacf(weatherarima, lag.max=20)
adf.test(weatherarima)

components <- decompose(weatherarima)
components
plot(components)

# ARIMA
fitweatherarima<-auto.arima(weatherarima, trace=TRUE, test="kpss", ic="bic")
fitweatherarima
confint(fitweatherarima)
plot(weatherarima,type='l')
title('Mean temperature')

# Forecasted Values From ARIMA
forecastedvalues=forecast(fitweatherarima,h=183)
forecastedvalues
plot(forecastedvalues)

# Test Values
test=mydata$meant[733:915]
test

library(Metrics)
rmse(forecastedvalues$mean, test)
mean(test)