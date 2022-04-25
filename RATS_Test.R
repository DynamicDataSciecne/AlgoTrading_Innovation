###########################################################################################################################
# Function to implement the proposed robust trading strategy using DDVFI and the traditional one using KFVEI to test sample
# Outputs: this function will provide the results (Table 2) presented in manuscript and in the supplementary part
###########################################################################################################################

# data: a number of stocks used
# y: stock as response variable
# x: stocks as features
# delta: number to generate constant state covariance matrix
# Ve: constant innovation variance
# p.optimal: vector of optimal value of threshold p from the training sample using DDVFI and KFVEI method, respectively
# window.size: length of rolling window size to forecast innovation volatility

RATS_test<- function(data, y, x, delta = 0.0001, Ve = 0.001, p.optimal, window.size = 0) {
  data<- data    
  # Implementation of maximum informative filter algorithm
  res <- kalman_iteration(data = data, y=y, x=x, delta = delta, Ve= Ve) 
  # Extract results
  beta <- xts(res[[1]], order.by=index(data))

  # DD-EWMA innovation volatility forecasts
  vol <- NA # volatility
  RMSE.algo<- NA 
  nu <- res[[4]] # innovation
  alpha<-seq(0.01, 0.5, 0.01) # range of alpha
  for(i in 1: (nrow(data)-window.size)) {
    result <- DD_volatility (nu[i:(window.size+i-1)], 20, alpha = alpha)
    vol[i] <-result[1]
    RMSE.algo<- result[2]
  }
  
  # plot trade signals
  nu <- xts(nu, order.by=index(data))
  sqrtQ <- xts(sqrt(res[[3]]), order.by=index(data)) # KFVEI
  sqrtQ[1:window.size] <- NA
  vol <- xts(c(rep(NA, window.size), vol), order.by=index(data)) # DDVFI

  ## Proposed robust pairs trading strategy (DDVFI)
  # create optimal trading signals based on robust DDVFI method 
  p <- p.optimal[1]
  signals_DDVFI <- merge(nu, p*vol, -p*vol)
  colnames(signals_DDVFI) <- c("nu", "vol", "negvol")

  ## Calculate the cumulative profit using optimal trading signals based on DDVFI
  # Implementation of profit and loss function to calculate cumulative profit
  profit.loss<- PnL(signals_DDVFI, nu, beta, x, y)
  profit_DDVFI<- sum (na.omit(profit.loss))
  profit_DDVFI
  ASR_DDVFI<- sqrt(252)*mean(na.omit(profit.loss))/sd(na.omit(profit.loss))
  ASR_DDVFI
  
  ## Traditional pairs trading strategy (KFVEI)
  # create optimal trading signals based on robust KFVEI method 
  p <- p.optimal[2]
  signals_KFVEI <- merge(nu, p*sqrtQ, -p*sqrtQ)
  colnames(signals_KFVEI) <- c("nu", "sqrtQ", "negsqrtQ")
  
  ## Calculate the cumulative profit using optimal trading signals based on KKVEI
  # Implementation of profit and loss function to calculate cumulative profit
  profit.loss<- PnL(signals_KFVEI, nu, beta, x, y)
  profit_KFVEI<- sum (na.omit(profit.loss))
  profit_KFVEI
  ASR_KFVEI<- sqrt(252)*mean(na.omit(profit.loss))/sd(na.omit(profit.loss))
  ASR_KFVEI
  ## Save the results
  res_DDVFI<- data.frame(popt_DDVFI=p.optimal[1], ASR_DDVFI, profit_DDVFI)
  res_KFVEI<- data.frame(popt_KFVEI=p.optimal[2], ASR_KFVEI, profit_KFVEI)
  results<- cbind(res_DDVFI, res_KFVEI)
  return(results)
}



