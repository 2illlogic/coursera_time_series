
arima.search <- function(ts, p, d, q, P, D, Q, s, max.par=6){
  for(i in p){
    for(j in q){
      for(I in P){
        for(J in Q){
          if(i+d+j+I+D+J <= max.par){
            model <- arima(ts
                           ,order = c(i,d,j)
                           ,seasonal = list(order = c(I,D,J)
                                            ,period = s)
                           )
            pval <- Box.test(model$residuals, lag=log(length(model$residuals)))
            sse <- sum(model$residuals^2)
            cat(i,d,j,I,D,J,s, 'AIC=', model$aic, ' SSE=', sse, ' p-Value=', pval$p.value, '\n') 
          }
        }
      }
    }
  }
}