# JAGS model code  ------------------------------------------------------------------
jags_mod2 <- function(){
  # Loop through career
  for(i in 1:ns) {
    # season-specific parameter
    # ita[i] ~ dpois(m)
    ita[i] <- t[i]
    
    # parameter for negative binomial distribution
    r[i] <- ita[i]*m
    p[i] <- t[i]/(ita[i] + t[i])
    
    # sample home run from negative binomial distribution
    hr[i] ~ dnegbin(p[i], r[i])
    }
  
  # vague prior for true home run rate 
  m ~ dunif(0, 1)
}

# fixed effect on home run rate ------------------------------------------------------------
jags_mod3 <- function(){
  # Loop through season
  for(i in 1:ns) {
    # Poisson deviance
    D[i] <- -2*t[i]*(y[i] - lambda - y[i]*log(y[i]/lambda))
    
    # zero one trick to sample from Poisson QL distribution
    zero[i] ~ dpois(phi[i])
    phi[i] <- -0.5*(log(g[i]) - log(2*ppi) - log(t[i]*y[i]) - g[i]*D[i])
  }
  
  # informative prior for g[i]
  for(i in 1:ns){
    g[i] ~ dgamma(nu/2, nu/2)
  }
  
  # vague prior for lambda, true home run rate
  lambda ~ dunif(-100, 100)
}

# random effect on home run rate ------------------------------------------------------------
jags_mod4 <- function(){
  # Loop through career
  for(i in 1:ns) {
    # Quadratic log-linear model
    log(lambda[i]) <- beta[1] + beta[2]*i + beta[3]*pow(i, 2)

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
  
  # vague prior for log-linear model coefficient
  beta[1] ~ dnorm(0, 0.0001)
  beta[2] ~ dnorm(0, 0.0001)
  beta[3] ~ dnorm(0, 0.0001)
}

# random effect on home run rate ------------------------------------------------------------
jags_mod5 <- function(){
  # loop through player
  for(i in 1:np) {
    # loop through season
    for(j in 1:ns[i]){
      # Quadratic log-linear model
      log(lambda[i, j]) <- beta1[i] + beta2[i]*j + beta3[i]*pow(j, 2)
      # log(lambda[i, j]) <- beta1 + beta2*j + beta3*pow(j, 2)
      
      # Poisson deviance for Poisson QL 
      D[i, j] <- -2*t[i, j]*(y[i, j] - lambda[i, j] - y[i, j]*log(y[i, j]/lambda[i, j]))
      
      # zero one trick to sample from Poisson QL
      # http://www.medicine.mcgill.ca/epidemiology/Joseph/courses/common/Tricks.html
      zero[i, j] ~ dpois(phi[i, j])
      phi[i, j] <- -0.5*(log(g[i, j]) - log(2*ppi) - log(t[i, j]*y[i, j]) - g[i, j]*D[i, j])
      
      # informative prior for g[i] 
      g[i, j] ~ dgamma(nu/2, nu/2)
    }
  }
  
  # # informative prior for g[i] 
  # for(k in 1:ns){
  #   g[k] ~ dgamma(nu/2, nu/2)
  # }
  
  # player-specific effect
  for(k in 1:np){
    # vague prior for log-linear model coefficient
    beta1[k] ~ dnorm(0, 0.0001)
    beta2[k] ~ dnorm(0, 0.0001)
    beta3[k] ~ dnorm(0, 0.0001)
    # p_eff[k] ~ dnorm(0, 0.0001)
  }
  
  # # vague prior for log-linear model coefficient
  # beta1 ~ dnorm(0, 0.0001)
  # beta2 ~ dnorm(0, 0.0001)
  # beta3 ~ dnorm(0, 0.0001)
}