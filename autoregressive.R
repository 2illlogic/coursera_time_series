set.seed(2017)

#use arima to simulate X_t = Z_t + 0.7X_(t-1) + 0.2X_(t-2)
X.ts <- arima.sim(list(ar = c(0.7,0.2)),n=1000)

par(mfrow=c(2,1))
plot(X.ts, main='AR(2) Time Series, coeffs=c(0.7,0.2)')
X.acf<-acf(X.ts, main='Autocorrelation of AR(2) Times Series')

#coeffs = c(0.2, 0.7)
X.ts <- arima.sim(list(ar = c(0.2,0.7)),n=1000)

par(mfrow=c(2,1))
plot(X.ts, main='AR(2) Time Series, coeffs=c(0.2,0.7)')
X.acf<-acf(X.ts, main='Autocorrelation of AR(2) Times Series')

#coeffs = c(0.5,0.5) - non-stationary
X.ts <- arima.sim(list(ar = c(0.49,0.5)),n=1000)

par(mfrow=c(2,1))
plot(X.ts, main='AR(2) Time Series, coeffs=c(0.49,0.5)')
X.acf<-acf(X.ts, main='Autocorrelation of AR(2) Times Series')

#better way w/paste()
Phi <- c(0.5,-0.4)
X.ts <- arima.sim(list(ar = Phi),n=1000)
par(mfrow=c(2,1))
plot(X.ts, main=paste('AR(2) Time Series, coeffs=c(',Phi[1],',',Phi[2],')'))
X.acf<-acf(X.ts, main='Autocorrelation of AR(2) Times Series')

#generalized to study behavior
phi.seed<-0.1
par(mfrow=c(4,1))
for (i in 1:4){
  Phi <- c(2*i*phi.seed,phi.seed)
  X.ts <- arima.sim(list(ar=Phi),n=1000)
  X.acf<-acf(X.ts, main=paste('Autocorrelation of AR(2) w/ phi1=',Phi[1],', phi2=',Phi[2]))
}
for (i in 1:4){
  Phi <- c(phi.seed,2*i*phi.seed)
  X.ts <- arima.sim(list(ar=Phi),n=1000)
  X.acf<-acf(X.ts, main=paste('Autocorrelation of AR(2) w/ phi1=',Phi[1],', phi2=',Phi[2]))
}

Phi <- c(1)
X.ts <- arima.sim(list(ar = Phi),n=1000)
par(mfrow=c(2,1))
plot(X.ts, main=paste('AR(2) Time Series, phi=',Phi[1]))
X.acf<-acf(X.ts, main='Autocorrelation of AR(2) Times Series')

