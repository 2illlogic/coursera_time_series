library(astsa)

# create random process and set weights
noise <- rnorm(10000)
weights <- c(0.7, 0.2)

# moving average algorithm
ma_2 <- NULL
for(i in 3:10000){
  ma_2[i] <- noise[i] + weights[1]*noise[i-1] +
    weights[2]*noise[i-2]
}

# shift left by 2 units i.e. cut off initial terms 
# because the process is undefined for them
ma_process <- ts(ma_2[3:10000])

par(mfrow=c(2,1))
plot(ma_process, main='Moving Average Process of Order 2', ylab=' ')
acf(ma_process, main='Corellogram of a Moving Average Process of Order 2')
