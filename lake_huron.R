library(astsa)

data <- LakeHuron

plot(data,main='Level of Lake Huron 1875-1972',ylab='level (feet)')

#use 'diff' function to de-trend the data
d.data <- diff(data)
d.data.mean.zero <- d.data-mean(d.data)

par(mfrow=c(3,1))
plot(d.data.mean.zero,main='De-trended Level of Lake Huron 1875-1972',ylab='level (feet)')
acf(d.data.mean.zero,main='Lake Huron autocorrelation',lag.max=25)
pacf(d.data.mean.zero,main='Lake Huron partial autocorrelation',lag.max=25)
dev.off()
#PACF with significant coefficient at lags 2 and 20
#still includes cyclicality but assume AR(2) model

#setting vector r = sample autocorrelations
r <- acf(d.data.mean.zero, plot=F)$acf[2:3]

#examine Yule-Walker matrix
R <- yw_matrix(r)

#estimate model coefficients from sample autocorrelations
phi.hat <- yw_coeffs(r)

#set c0 = sample autocovariance
c0 <- acf(d.data.mean.zero, type='covariance', plot='F')$acf[1]

#estimate the variance in the noise
var.hat <- c0*(1-sum(phi.hat*r))

#reverse differencing to model original dataset
phi.hat.orig <- dediff_coeffs(phi.hat)

k <- length(data)
#simulation of model
set.seed(1880)
sim <- arima.sim(list(ar = phi.hat.orig), innov=rnorm(n=k,sd=sqrt(var.hat)), n=k)
sim <- sim + mean(data)
#gotta add the mean back in somehow!!
plot(data, col='blue')
lines(sim, col='red')
