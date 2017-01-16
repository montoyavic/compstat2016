


likelihood <- function(yobs, xind, theta){
  a = theta[1]
  b = theta[2]
  s = theta[3]
  
  muis = a + b*xind
  
  singleloglike = dnorm(yobs, mean = muis, sd = s, log = F)
  loglike = sum(singleloglike)
  
  return(loglike)
}



prior <- function(theta, paramsa, paramsb, paramss){
  a = theta[1]
  b = theta[2]
  s = theta[3]
  
  aprior <- dnorm(a, mean = paramsa[1], sd = paramsa[2], log = T)
  bprior <- dnorm(b, mean = paramsb[1], sd = paramsb[2], log = T)
  sdprior <- dgamma(s, shape = paramss[1], rate = paramss[2], log = T)
  
  return(aprior + bprior + sdprior)
}


posterior <- function(yobs, xind, theta,
                      paramsa, paramsb, paramss){
  loglike <- likelihood(yobs = yobs, xind = xind, theta = theta)
  logprior <- prior(theta = theta, paramsa = paramsa,
                    paramsb = paramsb, paramss = paramss)
  return ( loglike + logprior)
}


proposalfun <- function(theta, jump){
  # propuesta <-  mvrnorm(1, mu = theta, Sigma = rep(jump, 3)*diag(1, 3) )
  # while(propuesta[3]<0){
  #   propuesta <- mvrnorm(1, mu = theta, Sigma = rep(jump, 3)*diag(1, 3) )
  # }
  propuesta <- rep(NA, 3)
  propuesta[1] <- rnorm(1, theta[1], jump)
  propuesta[2] <- rnorm(1, theta[2], jump)
  propuesta[3] <- abs(rnorm(1, theta[3], jump) + .001)
  # print(propuesta[3])
  
  return(propuesta)
}


run_metropolis_mcmc_r <- function(inits, nsims,
                                  yobs, xind,
                                  paramsa, paramsb, paramss){
  
  # print(class(inits))
  
  chain = matrix( 0, nrow = nsims + 1, ncol =  3)
  chain[1,] = inits
  
  for (i in 1:nsims){
    proposal = proposalfun(chain[i,], jump = 10)
    currentpost <- posterior(yobs,
                             xind, 
                             chain[i, ],
                             paramsa,
                             paramsb,
                             paramss)
    
    proposalpost <- posterior(yobs,
                              xind, 
                              proposal,
                              paramsa,
                              paramsb,
                              paramss)
    
    probab <-  proposalpost - currentpost
    # print( proposal )
    # print( paste(proposalpost, currentpost) )
    
    if ( log(runif(1)) < probab){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
    
  }
  
  return(chain)
}





RhatFun <- function(cadenas.df, n.sims, n.cadenas){
  
  tab.rhat <- cadenas.df %>% 
    group_by(param, cadena) %>% 
    # Calculo de W
    summarize(sj = var(sim)) %>% 
    group_by(param) %>% 
    summarize(w = sum(sj)/n.cadenas) %>% 
    # Calculo de B
    left_join(
      cadenas.df %>% 
        group_by(param) %>% 
        mutate(betaprom.tot = mean(sim)) %>% 
        group_by(param, cadena) %>% 
        summarize(betaprom.cad = mean(sim),
                  betaprom.tot = unique(betaprom.tot)) %>% 
        group_by(param) %>% 
        summarize(sbeta = sum(betaprom.cad-betaprom.tot)^2, 
                  b = n.sims*sbeta*n.sims/(n.cadenas-1)),
      by = 'param'
    ) %>% 
    # Calculo de varianza y rhat
    group_by(param) %>% 
    mutate(var.beta = (1- 1/n.sims)*w + (1/n.sims)*b,
           Rhat = sqrt(var.beta/w)) %>% 
    dplyr::select(param, w, b, Rhat)
  
  return(tab.rhat)
}