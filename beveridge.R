library(astsa)
library(stats)
rm(list=ls())

#remember stringsAsFactors=FALSE when importing CSV so
#is read as such
beveridge <- read.csv('../../data_sets/beveridge-wheat-price-index-1500.csv', header=TRUE, stringsAsFactors=FALSE)
beveridge <- beveridge[1:370,]

beveridge.ts <- ts(as.numeric(beveridge[,2]), start=1500)

plot(beveridge.ts, ylab='price', main='Beveridge Wheat Price Data')

beveridge.ma <- stats::filter(beveridge.ts, rep(1/31, 31), sides=2)

lines(beveridge.ma, col='red')

par(mfrow=c(3,1))
Y <- beveridge.ts/beveridge.ma
plot(Y, ylab='scaled price', main='Transformed Beveridge Wheat Price Data')
acf(na.omit(Y), 
    main='Autocorrelation Function of Transformed Beveridge Data')
acf(na.omit(Y), type='partial',
    main='Partial Autocorrelation of Transformed Beveridge Data')

ar(na.omit(Y), order.max=5)
