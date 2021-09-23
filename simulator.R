library(tidyverse)
library(Sim.DiffProc)

sim_gbm <- function(pdeath, grr, drift, vol, nsim) {
  
  investment <- 10^4
  
  final_amount <- vector('numeric', nsim)
  
  for (i in 1:nsim) {
    
    evolution <- GBM(N = 20, T = 1440 * 90, t0 = 0, x0 = 1, theta = drift, sigma = vol)
    
    returns <- evolution %>% log() %>% diff() %>% exp()
    
    min_returns <- returns
    
    min_returns[min_returns < (1 + grr)] <- 1 + grr
    
    current_amount <- rep(investment, 21)
    
    death <- runif(20)
    
    payments <- 1
    
    if (!exists('total_deaths')) total_deaths <- 0
    
    for (j in 1:20) {
      
      if (1 - pdeath > death[j]) {
        
        current_amount[j + 1] <- current_amount[j] * min_returns[j] + investment
        
        payments <- payments + 1
        
      } else {
        
        current_amount[(j + 1):20] <- current_amount[j] * min_returns[j]
        
        total_deaths <- total_deaths + 1
        
        break
        
      }
      
    }
    
    final_amount[i] <- current_amount[20] - payments * investment
    
  }
  
  return(list(final_amount, total_deaths))
  
}
