library(astsa)
library(forecast)
library(datasets)
rm(list=ls())
dev.off()

data <- BJsales 

plot(data, ylab='Sales', main='Box & Jenkins sales data')

data.diff <- diff(data)

plot(data.diff, ylab='Sales', main='Box & Jenkins sales data, d=1')
abline(h=mean(data.diff),col='red')

data.diff.2 <- diff(data.diff)

plot(data.diff.2, ylab='Sales', main='Box & Jenkins sales data, d=2')
abline(h=mean(data.diff.2),col='red')

pacf(data.diff.2)
acf(data.diff.2)

sarima(data,0,2,1,0,0,0)
sarima(data.diff.2,0,0,1,0,0,0)
