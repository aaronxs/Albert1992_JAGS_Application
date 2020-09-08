# random effect on home run rate ------------------------------------------------------------
jags_mod4 <- function(){
  # Loop through career
  for(i in 1:ns) {
    # Quadratic log-linear model
    log(lambda[i]) <- beta[1] + beta[2]*i + beta[3]*pow(i, 2) + p_eff[ID[i]]
    
    # Poisson deviance for Poisson QL 
    D[i] <- -2*t[i]*(y[i] - lambda[i] - y[i]*log(y[i]/lambda[i]))
    
    # zero one trick to sample from Poisson QL
    # http://www.medicine.mcgill.ca/epidemiology/Joseph/courses/common/Tricks.html
    zero[i] ~ dpois(phi[i])
    phi[i] <- -0.5*(log(g[i]) - log(2*ppi) - log(t[i]*y[i]) - g[i]*D[i])
  }
  
  # informative prior for g[i] 
  for(k in 1:ns){
    g[k] ~ dgamma(nu/2, nu/2)
  }
  
  # player-specific effect
  for(k in 1:np){
    p_eff[k] ~ dnorm(0, 0.0001)
  }
  
  # vague prior for log-linear model coefficient
  beta[1] ~ dnorm(0, 0.0001)
  beta[2] ~ dnorm(0, 0.0001)
  beta[3] ~ dnorm(0, 0.0001)
}