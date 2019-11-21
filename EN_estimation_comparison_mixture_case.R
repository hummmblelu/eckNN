## data generation
library(mvtnorm)

rmixgaussian<- function(n, p_1, mu_1, S_1, mu_2, S_2){
  n_1<- rbinom(1,size=n,prob=p_1)
  n_2<- n - n_1
  val_1<- rmvnorm(n_1, mu_1, S_1)
  val_2<- rmvnorm(n_2, mu_2, S_2)
  
  all_val <- rbind(val_1, val_2)      ## combine
  all_val <- all_val[sample(n,n),]  ## scramble order
  all_val
}

dmixgaussian<- function(X, p_1, mu_1, S_1, mu_2, S_2, log = F){
  if(log == T){
    d_1<- dmvnorm(X, mu_1, S_1)
    d_2<- dmvnorm(X, mu_2, S_2)
    
    out<- (d_1 * p_1) + (d_2 * (1 - p_1))
    
    log(out)
  }else{
    d_1<- dmvnorm(X, mu_1, S_1)
    d_2<- dmvnorm(X, mu_2, S_2)
    
    out<- (d_1 * p_1) + (d_2 * (1 - p_1))
  }
}

dmixgaussian_marginal<- function(X, p_1, mu_1, S_1, mu_2, S_2, log = F, dimension){
  if(log == T){
    d_1<- dnorm(X, mu_1, sqrt(S_1[dimension,dimension]))
    d_2<- dnorm(X, mu_2[dimension], sqrt(S_2[dimension,dimension]))
    
    out<- (d_1 * p_1) + (d_2 * (1 - p_1))
    
    log(out)
  }else{
    d_1<- dnorm(X, mu_1[dimension], sqrt(S_1[dimension,dimension]))
    d_2<- dnorm(X, mu_2[dimension], sqrt(S_2[dimension,dimension]))
    
    out<- (d_1 * p_1) + (d_2 * (1 - p_1))
  }
}

mixture_sample<- lapply(5:20, function(D){
  p_1<- .4
  
  lapply(1:10, function(t){
    mu_1<- rep(0, D)
    ss_1<- matrix(rnorm(D, rep(0, D), 3), 1, D)
    S_1<- t(ss_1) %*% ss_1 + diag(D)
    
    mu_2<-rep(10, D)
    ss_2<- matrix(rnorm(D, rep(0, D), 3), 1, D)
    S_2<- t(ss_2) %*% ss_2+ diag(1:D)
    
    X<- rmixgaussian(1000, p_1,mu_1, S_1, mu_2, S_2)
    
    samples_monte_carclo<- rmixgaussian(100000, p_1, mu_1, S_1, mu_2, S_2) ## samples for Monte Carlo estimation
    
    ## Monte Carlo Estimation
    tv_entropy<- -mean(dmixgaussian(samples_monte_carclo, p_1, mu_1, S_1, mu_2, S_2, log = T))
    
    tv_mi<- tv_entropy - sum(sapply(1:D, function(i)
      -mean(dmixgaussian_marginal(samples_monte_carclo, p_1, mu_1, S_1, mu_2, S_2, log = T, i))
    ))
    
    list(tv_entropy = tv_entropy, tv_mi = tv_mi, X = X)
  })
})

save(mixture_sample, file = 'mixture_sample.RData') ## save data (for safety)

## Entropy
## entropy estimation
load('mixture_sample.RData')
## ec-kNN
source('ec_knn.R')

ec<- sapply(1:16, function(d){
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- mixture_sample[[d]][[t]]$X
    tv<- mixture_sample[[d]][[t]]$tv_entropy
    entropy_ec(X, k = 25) - tv
  })
})

## classical kNN
classical_1<- sapply(1:16, function(d){
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- mixture_sample[[d]][[t]]$X
    tv<- mixture_sample[[d]][[t]]$tv_entropy
    FNN::entropy(X, k = 1)[1] - tv
  })
})

classical_5<- sapply(1:16, function(d){
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- mixture_sample[[d]][[t]]$X
    tv<- mixture_sample[[d]][[t]]$tv_entropy
    FNN::entropy(X, k = 5)[5] - tv
  })
})

classical_10<- sapply(1:16, function(d){
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- mixture_sample[[d]][[t]]$X
    tv<- mixture_sample[[d]][[t]]$tv_entropy
    FNN::entropy(X, k = 10)[10] - tv
  })
})

classical_15<- sapply(1:16, function(d){
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- mixture_sample[[d]][[t]]$X
    tv<- mixture_sample[[d]][[t]]$tv_entropy
    FNN::entropy(X, k = 15)[15] - tv
  })
})

classical_25<- sapply(1:16, function(d){
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- mixture_sample[[d]][[t]]$X
    tv<- mixture_sample[[d]][[t]]$tv_entropy
    FNN::entropy(X, k = 25)[25] - tv
  })
})

##kNN-bw
## bandwidth Gao et al, 2016
library(reticulate)
source_python('lnn/lnn.py')

knn_bw<- sapply(1:16, function(d){
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- mixture_sample[[d]][[t]]$X
    tv<- mixture_sample[[d]][[t]]$tv_entropy
    entropy(X) - tv
  })
})

save(ec, classical_1, classical_5, classical_10, classical_15, classical_25, knn_bw, file = 'mixture_sample_entropy.RData')