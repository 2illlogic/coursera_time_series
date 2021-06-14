library(datasets)
library(astsa)
rm(list = ls())
dev.off()

data.ts <- ts(USAccDeaths, start = c(1973,1), frequency = 12)

plot(data.ts, main='US Accidental Deaths from 1973-1978')

data.diff <- diff(data.ts, 12)

plot(data.diff, main='Differenced US Accidental Deaths from 1973-1978')

data.diff <- diff(data.diff)
par(mfrow=c(2,1))
acf(data.diff, lag.max = 24)
pacf(data.diff, lag.max = 24)

model <- sarima(data.ts,0,1,1,0,1,1,12)
#to view p-values of coefficients
model$ttable

sarima.for(data.ts,12,0,1,1,0,1,1,12)
