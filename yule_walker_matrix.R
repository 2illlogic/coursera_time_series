library(astsa)
#r <- c(0.8,0.6,0.2)

yw_matrix <- function(r){
  p <- length(r)
  R <- matrix(1,p,p)
  for (i in 1:p){
    for (j in 1:p){
      d = abs(i-j)
      if (d!=0){
        R[i,j] <- r[d]
      }
    }
  }  
  return(R)
}

yw_coeffs <- function(r){
  b <- matrix(r,length(r),1,byrow=TRUE)
  R <- yw_matrix(r)
  return(solve(R,b))
}

dediff_coeffs <- function(phi.hat){
  k <- length(phi.hat)
  phi.hat.orig <- NULL
  phi.hat.orig[1] = 1 + phi.hat[1]
  phi.hat.orig[k+1] = phi.hat[k]
  for (i in 2:k){
    phi.hat.orig[i] <- phi.hat[i]-phi.hat[i-1]
  }
  return(phi.hat.orig)
}

#c0 <- 5
#sig.hat <- c0*(1-sum(phi.hat*r))
