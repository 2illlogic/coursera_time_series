library(astsa)
library(forecast)

# (1) Simple Exponential Smoothing
#  (i)  No trend
#  (ii) No seasonality
# how to get data straight off the internet
data.raw <- scan('http://robjhyndman.com/tsdldata/hurst/precip1.dat', skip=1)
data.ts <- ts(data.raw, start=c(1813))

# basic preliminary analysis
par(mfrow=c(1,2))

hist(data.raw, main='Annual London Rainfall 1813-1912'
     ,xlab='rainfall (in.)')

qqnorm(data.raw, main='Normal Plot of Annual London Rainfall')

qqline(data.raw)

# basic time series analysis
par(mfrow=c(2,1))
plot(data.ts, main='Annual London Rainfall 1813-1913'
     ,xlab='rainfall (in.)')
acf(data.ts)
# no structure - just a mean
auto.arima(data.ts)
# auto.arima confirms lack of structure

dev.off()
# # Forecasting! # #
n <- length(data.raw)
alpha.values <- seq(0.001,0.999,by=0.001)
m <- length(alpha.values)
forecast.sse <- NULL

# first loop optimizes alpha
for(j in 1:m){
  forecast.values <- NULL
  alpha <- alpha.values[j]
    
  #naive first forecast:
  forecast.values[1] <- data.raw[1]
    
  #loop for recursive forecast values
  # to get forecast we have to plug in optimized alpha 
  # and loop this part by itself - make function?
  for(i in 1:n){
    forecast.values[i+1] <- alpha*data.raw[i] + (1-alpha)*forecast.values[i]
  }
  forecast.sse[j] <- sum((data.raw - forecast.values[1:n])^2)  
}

plot(forecast.sse ~ alpha.values, main='Optimal alpha value minimizes SSE')
sse.min.index <- which.min(forecast.sse)
alpha.values[sse.min.index]
# best alpha is 0.024

#paste('Forecast for time',n+1,'=',forecast.values[n+1])

# easier way - yields optimized forecast for ts 
# w/no trend or seasonal component
HoltWinters(data.ts, beta=FALSE, gamma=FALSE)

# (2) Double Exponential Smoothing
#  (i)  Trend
#  (ii) No seasonality
rm(list=ls())
dev.off()

data.raw <- read.csv('../../data_sets/volume-of-money-abs-definition-m.csv'
                     ,stringsAsFactors = FALSE
                     ,col.names = c('month','money'))
data.raw <- head(data.raw,-1) 
data.raw$money <- as.numeric(data.raw$money)
data.ts <- ts(data.raw$money, start=c(1960,2), frequency = 12)

par(mfrow=c(3,1))
plot(data.ts, main='Monthly Volume of Money')
acf(data.ts)
pacf(data.ts)

model <- HoltWinters(data.ts, gamma=FALSE)

par(mfrow=c(1,2))
plot(data.ts)
plot(model)

# (3) Triple Exponential Smoothing
#   (i)  Trend
#   (ii) Seasonality 
dev.off()
data <- AirPassengers
plot(data, main='International Airline Passengers 1949-1960')

model <- HoltWinters(data)
model
model$coefficients['a']

# Sunspots
data <- sunspots
plot(data, main='Monthly Mean Relative Sunspot Numbers 1749-1983')

model <- HoltWinters(data)
model
library(zoo)
as.yearmon(max(time(data)))

coeff <- model$coefficients
jan.1984 <- coeff['a'] + 1*coeff['b'] + coeff['s1']
jan.1984

library(forecast)
forecast <- forecast(model)$mean
plot(c(data,forecast), type='l')

#library(ggplot2)
#ggplot(data=c(data,forecast), aes(x=date))+geom_line(mapping=aes(color=(time(c(data,forecast))>=1984)))
