library(astsa)
library(forecast)

#(1)Johnson & Johnson
data <- JohnsonJohnson
plot(data, main='Quarterly Earnings of Johnson & Johnson')

#log-return transformation and differencing
data.log <- diff(diff(log(data)),4)
plot(data.log, main='differenced log-return of Johnson & Johnson Earnings')

#Ljung-Box test
Box.test(data.log, lag=log(length(data.log)))
#p-value = 0.0004658 => there is autocorrelation with previous lags

#ACF & PACF
par(mfrow=c(2,1))
acf(data.log)
#suggests q=0,1, Q=0,1 (non-seasonal and seasonal MA parts)
pacf(data.log)
#suggests p=0,1, P=0,1 (non-seasonal and seasonal AR parts)

#try auto.arima
model.1 <- auto.arima(log(data)
                      ,d=1
                      ,D=1
                      ,start.p=0,max.p=1
                      ,start.q=0, max.q=1
                      ,start.P=0,max.P=1
                      ,start.Q=0,max.Q=1)
#ARIMA(0,1,1,0,1,1)[4] w/ma1=-0.6809, sma1=-0.3146

#examine results with SARIMA function
#my model
sarima(log(data),p=0,d=1,q=1,P=0,D=1,Q=1,S=4)
#model from course
sarima(log(data),p=0,d=1,q=1,P=1,D=1,Q=0,S=4)

#forecasting
model.1 <- arima(log(data),order=c(0,1,1),seasonal=list(order=c(1,1,0),period=4))
plot(forecast(model.1,h=10))

dev.off()
data <- window(JohnsonJohnson,end=c(1978,4))
data <- log(data)
plot(data,type='l')
data.diff <- diff(diff(data),4)
Box.test(data.diff, lag=log(length(data.diff)))
par(mfrow=c(2,1))
acf(data.diff)
pacf(data.diff)
model <- auto.arima(data
                      ,d=1
                      ,D=1
                      ,start.p=0,max.p=1
                      ,start.q=0,max.q=1
                      ,start.P=0,max.P=1
                      ,start.Q=0,max.Q=1
                    )
model
par(mfrow=c(2,1))
plot(forecast(model,h=8))
plot(log(JohnsonJohnson))
ts.plot(forecast(model,h=8),log(JohnsonJohnson),gpars=list(col=c('blue','red')))
#?????????????
rm(list=ls())

#Milk Production
#preprocessing
data.raw <- read.csv('../../data_sets/monthly-milk-production-pounds-p.csv',stringsAsFactors = FALSE)
#clean up junk in last row
data.raw <- head(data.raw,-1)
#fix column names for easy reference
colnames(data.raw) <- c('month','milk')
#turn into time series w/settings
## start = c(year, month/quarter)
## frequency = 12 for months/4 for quarters
### set frequency = 1 - otherwise makes acf & pacf hard to interpret
data.ts <- ts(data.raw['milk'], start = c(1962,1), frequency = 12)

#time plot
plot(data.ts, main = 'Milk Production Time Series')
#definitely not stationary - will require differencing
#also shows seasonal trend - will require seasonal differencing
data.diff <- diff(diff(data.ts),12)
plot(data.diff, main = 'Difference Milk Production Time Series')
#now stationary
dev.off()
par(mfrow=c(2,1))
acf(data.diff, type = 'correlation', lag.max = 50)
# spike at 12, 24, 36 => q=0, Q=0,1,2,3
pacf(data.diff, lag.max = 50)
# spikes at 12 & 24 => p=0, P=0,1,2

#non-automated ARIMA loop just to be safe
#write as function later
for(Q in 0:3){
  for(P in 0:2){
    if(P+Q<=4){
      model <- arima(data.ts
                     ,order = c(0,1,0)
                     ,seasonal = list(order = c(P,1,Q), period = 12))
      pval <- Box.test(model$residuals, lag=log(length(model$residuals)))
      sse <- sum(model$residuals^2)
      cat(0,1,0,P,1,Q,12, 'AIC=', model$aic, ' SSE=', sse, ' p-Value=', pval$p.value, '\n')
    }
  }
}
#indicates best model is ARIMA(0,1,0,0,1,1,12)

sarima(data.ts, 0,1,0,0,1,1,12)

model <- arima(data.ts, order = c(0,1,0), seasonal = list(order = c(0,1,1), period = 12))
dev.off()
plot(forecast(model), main = 'Past Milk Production & 2-year Forecast')

#testing new function
arima.search(data.ts,0:0,1,0:0,0:2,1,0:3,12,6)

#-----Souvenir Shop Sales--------------------------
data.raw <- read.csv('../../data_sets/monthly-sales-for-a-souvenir-sho.csv'
                     ,col.names = c('month','sales')
                     ,stringsAsFactors = FALSE
                    )
View(data.raw)
per <- 12
data.raw <- head(data.raw,-1)
#use col.names argument instead
#colnames(data.raw) <- c('month','sales')
data.raw['sales'] <- as.numeric(data.raw$sales)
data.ts <- ts(data.raw['sales']
              ,start = c(1987,1)
              ,frequency = per
              )
plot(data.ts, main='Monthly Sales')
# requires log transform to normalize variance
data.ts.log <- log(data.ts)
# requires non-seasonal and seasonal differencing
d <- 1
D <- 1

data.diff <- diff(diff(data.ts.log), per)
plot(data.diff, main='Differenced Monthly Sales')
par(mfrow=c(2,1))

acf(data.diff, lag.max=50)
q <- c(0,1)
Q <- c(0,1,2,3)

pacf(data.diff, lag.max=50)
p <- c(0,1)
P <- c(0,1)

arima.search(data.ts.log,p,d,q,P,D,Q,per)
#best model is 1,1,0,0,1,1

model <- arima(data.ts.log
               ,order = c(1,1,0)
               ,seasonal = list(order=c(0,1,1), frequency=12))

plot(forecast(model))

forecast(model)

sarima(data.ts.log,1,1,0,0,1,1,12)

forecast <- sarima.for(data.ts.log,12,1,1,0,0,1,1,12)

# putting all the pieces together - 
## note the method of extracting the forecasted data values
## also note that this suggest a method for joining 
## two time series end to end
par(mfrow=c(3,1))
plot.ts(data.ts, col='red')
plot.ts(exp(forecast$pred),col='blue')
plot.ts(c(data.ts,exp(forecast$pred)),col='green')
