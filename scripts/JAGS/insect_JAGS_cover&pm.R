## insect index on sticky traps evaluated in BACI experiment
##
## First edit: 2020225
## Last edit: 2020429
##
## Author: Julian Klein

model{
  
  ## Likelihood: ---------------------------------------------------------------
  
  ## Process model:
  for(i in 1:nobs) {
    ## Stochastic model:
    resp[i] ~ dgamma(shape[i], rate[i])
    # sim[i] ~ dgamma(shape[i], rate[i])
    ## Moment matching:
    shape[i] <- max(0.0001, mu[i]^2/sigma^2)
    rate[i] <- max(0.0001, mu[i]/sigma^2)
    ## Deterministic model:
    log(mu[i]) <- a[treat[i],exp[i]] + e_block[block[i]] +
                  b_2018*year_2018[i] + 
                  b_2019*year_2019[i]
  }
  
  for(j in 1:max(block)){e_block[j] ~ dnorm(0, 1/sigma_block^2)}
  
  ## Priors: -------------------------------------------------------------------
  
  ## Process model:
  for(m in 1:max(treat)){
    for(n in 1:max(exp)){
      a[m,n] ~ dnorm(0, 0.001)
  }}
  sigma ~ dt(0, pow(2.5,-2), 1)T(0,)
  b_2018 ~ dnorm(0, 0.001)
  b_2019 ~ dnorm(0, 0.001)
  sigma_block ~ dt(0, pow(2.5,-2), 1)T(0,)
  
  # ## Model validation: ---------------------------------------------------------
  # 
  # ## Bayesian p-value:
  # mean_obs <- mean(resp[])
  # mean_sim <- mean(sim[])
  # p_mean <- step(mean_sim - mean_obs)
  # 
  # ## Coefficient of variation:
  # cv_obs <- sd(resp[])/mean_obs
  # cv_sim <- sd(sim[])/mean_sim
  # p_cv <- step(cv_sim - cv_obs)
  # 
  # ## Model fit:
  # for(i in 1:nobs){
  #   sq[i] <- (resp[i] - shape[i]/rate[i])^2
  #   sq_sim[i] <- (sim[i] - shape[i]/rate[i])^2
  # }
  # 
  # fit <- sum(sq[])
  # fit_sim <- sum(sq_sim[])
  # p_fit <- step(fit_sim - fit)
  # 
  ## Posteriors: ---------------------------------------------------------------

  for(m in 1:max(treat)){
    for(n in 1:max(exp)){
      log(a_bt[m,n]) <- a[m,n]
  }}
  
  ## BACI indicators:
  for(o in eval){
    CI_div[o] <- abs(a_bt[o,2]-a_bt[ref,2]) - abs(a_bt[o,1]-a_bt[ref,1])
    CI_ctr[o] <- abs(a_bt[o,2]-a_bt[o,1]) - abs(a_bt[ref,2]-a_bt[ref,1])
    BACI[o] <- (a_bt[o,2]-a_bt[o,1]) - (a_bt[ref,2]-a_bt[ref,1])
  }
  
}

