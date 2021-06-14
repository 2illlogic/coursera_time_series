library(astsa)
library(forecast)
dev.off()
#preprocessing
##will need to change this relative file path 
data <- read.csv('../../data_sets/daily-total-female-births-in-cal.csv'
                 ,header=TRUE
                 ,stringsAsFactors = FALSE)
data <- head(data,-1)
colnames(data) <- c('date','female_births')
data$date <- as.Date(data$date)

#plot to check trend
plot(data
     ,main='Daily Female Births in CA'
     ,xlab='Date'
     ,ylab='Births'
     ,type='l')
#appears to exhibit some trend - use differencing
data.diff <- data.frame(date=data$date[2:365],births=diff(data$female_births))
plot(data.diff
     ,main='Daily Female Births in CA'
     ,xlab='Date'
     ,ylab='Births'
     ,type='l')

#Ljung-Box test for correlation
Box.test(data$female_births, lag=log(length(data$date)))
#p-value = 2.088e-06 indicates that there is autocorrelation

#check correlations to determine orders of MA & AR parts
par(mfrow=c(2,1))
acf(data.diff$births, main='ACF of differenced data',50)
#MA orders: 2 or 21
pacf(data.diff$births, main='PACF of differenced data',50)
#AR orders: 7 or 21

#find model using forecast::auto.arima
auto.arima(data$female_births,d=1,approximation = FALSE)
auto.arima(data.diff$births,d=0,approximation = FALSE)
#unsurprisingly they are the same
#this result differs from the search method of the 
#course but gets better results by AIC

#fit model
sarima(data$female_births,1,1,1,0,0,0)
