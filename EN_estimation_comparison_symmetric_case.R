## entropy estimation comparison using symetric data
## data generation
library(mvtnorm)

sym_sample<- lapply(5:40, function(D){
  mu<- rep(0, D)
  S<- diag(D)
  
  lapply(1:10, function(t){
    ss<- matrix(rnorm(D, rep(0, D), 3), 1, D)
    S<- t(ss) %*% ss + diag(D)
    
    tv_entropy<- .5 * log(det(2*pi*exp(1)*S))
    
    tv_mi<- tv_entropy - sum(sapply(1:D, function(i)
      .5 * log(2*pi*exp(1)*S[i,i])
    ))
    
    X<- rmvnorm(1000, mu, S)
    
    tv<- .5 * log(det(2*pi*exp(1)*S))
    
    list(tv_entropy = tv_entropy, tv_mi = tv_mi, X = X)
  })
})

## save data (for safety)
save(sym_sample, file = 'sym_sample.RData') 

## entropy estimation
## ec-kNN
source('ec_knn.R')

set.seed(1)

ec<- sapply(1:16, function(d){
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- sym_sample[[d]][[t]]$X
    tv<- sym_sample[[d]][[t]]$tv_entropy
    entropy_ec(X, k = 25) - tv
  })
})

save(ec, sym_sample, file = 'sym_sample_entropy.RData')

## classical kNN
classical_1<- sapply(1:16, function(d){
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- sym_sample[[d]][[t]]$X
    tv<- sym_sample[[d]][[t]]$tv_entropy
    FNN::entropy(X, k = 1)[1] - tv
  })
})

classical_5<- sapply(1:16, function(d){
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- sym_sample[[d]][[t]]$X
    tv<- sym_sample[[d]][[t]]$tv_entropy
    FNN::entropy(X, k = 5)[5] - tv
  })
})

classical_10<- sapply(1:16, function(d){
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- sym_sample[[d]][[t]]$X
    tv<- sym_sample[[d]][[t]]$tv_entropy
    FNN::entropy(X, k = 10)[10] - tv
  })
})

classical_15<- sapply(1:16, function(d){
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- sym_sample[[d]][[t]]$X
    tv<- sym_sample[[d]][[t]]$tv_entropy
    FNN::entropy(X, k = 15)[15] - tv
  })
})

classical_25<- sapply(1:16, function(d){
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- sym_sample[[d]][[t]]$X
    tv<- sym_sample[[d]][[t]]$tv_entropy
    FNN::entropy(X, k = 25)[25] - tv
  })
})

save(ec, classical_1, classical_5, classical_10, classical_15, classical_25, sym_sample, file = 'sym_sample_entropy.RData')

## bandwidth Gao et al, 2016
load('sym_sample.RData')
library(reticulate)
source_python('lnn/lnn.py')

knn_bw<- sapply(1:16, function(d){
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- sym_sample[[d]][[t]]$X
    tv<- sym_sample[[d]][[t]]$tv_entropy
    entropy(X) - tv
  })
})

save(ec, classical_1, classical_5, classical_10, classical_15, classical_25, knn_bw, sym_sample, file = 'sym_sample_entropy.RData')