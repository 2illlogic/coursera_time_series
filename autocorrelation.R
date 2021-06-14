# generate a time series as random data points from a
# standard normal dist.
random_process <- ts(rnorm(100))

plot(random_process)

# run autocovariance coefficients
(acf(random_process, type='covariance'))

# run autocorrelation coefficients
(acf(random_process, type='correlation'))
