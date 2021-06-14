library(astsa)

# random walk algorithm for simulation
x <- NULL
x[1] <- 0
for(i in 2:1000){
  x[i] <- x[i-1] + rnorm(1)
}

# convert random walk to time series
random_walk <- ts(x)
plot(random_walk, main='A Random Walk')
(acf(random_walk))

# removing trend - converting to a completely random process
d <- diff(random_walk)
plot(d, main='A Purely Random Process')
(acf(d))


