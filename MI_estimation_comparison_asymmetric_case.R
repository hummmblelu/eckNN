## Mutual Information
## Run after entropy estimation
load('asym_sample.RData')

## ec-kNN
load('asym_sample_entropy.RData')
source('ec_knn.R')
ec_entropy<- sapply(1:16, function(d){
  D<- d+ 4
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    tv<- asym_sample[[d]][[t]]$tv_entropy
    ec[t, d] + tv ## re-use the results from entropy estimation
  })
})

ec<- sapply(1:16, function(d){
  D<- d+ 4
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    tv<- asym_sample[[d]][[t]]$tv_mi
    X<- asym_sample[[d]][[t]]$X
    ec_entropy[t,d] -  sum(sapply(1:D, function(d) FNN::entropy(X[,d], k = 25)[25])) - tv
  })
})

## classical
classical_1<- sapply(1:16, function(d){
  D<- d+ 4
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- asym_sample[[d]][[t]]$X
    tv<- asym_sample[[d]][[t]]$tv_mi
    FNN::entropy(X)[1] - sum(sapply(1:D, function(d) FNN::entropy(X[,d], k = 1)[1])) - tv
  })
})

classical_5<- sapply(1:16, function(d){
  D<- d + 4
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- asym_sample[[d]][[t]]$X
    tv<- asym_sample[[d]][[t]]$tv_mi
    FNN::entropy(X, k = 5)[5] - sum(sapply(1:D, function(d) FNN::entropy(X[,d], k = 5)[5])) - tv
  })
})

classical_10<- sapply(1:16, function(d){
  D<- d + 4
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- asym_sample[[d]][[t]]$X
    tv<- asym_sample[[d]][[t]]$tv_mi
    FNN::entropy(X, k = 10)[10] - sum(sapply(1:D, function(d) FNN::entropy(X[,d], k = 10)[10])) - tv
  })
})

classical_15<- sapply(1:16, function(d){
  D<- d + 4
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- asym_sample[[d]][[t]]$X
    tv<- asym_sample[[d]][[t]]$tv_mi
    FNN::entropy(X, k = 15)[15] - sum(sapply(1:D, function(d) FNN::entropy(X[,d], k = 25)[25])) - tv
  })
})

classical_25<- sapply(1:16, function(d){
  D<- d+ 4
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- asym_sample[[d]][[t]]$X
    tv<- asym_sample[[d]][[t]]$tv_mi
    FNN::entropy(X, k = 25)[25] - sum(sapply(1:D, function(d) FNN::entropy(X[,d], k = 25)[25])) - tv
  })
})

## KSG and LNC
load('asym_sample_mi.RData')

library(reticulate)
source_python('NPEET_LNC/lnc.py')
mi_Kraskov<- py_to_r(py_get_attr(py_to_r(MI), "mi_Kraskov"))

KSG<- sapply(1:16, function(d){
  D<- d+ 4
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- asym_sample[[d]][[t]]$X
    tv<- asym_sample[[d]][[t]]$tv_mi
    -mi_Kraskov(t(X)) - tv
  })
})

mi_LNC<- py_to_r(py_get_attr(py_to_r(MI), "mi_LNC"))
LNC<- sapply(1:16, function(d){
  D<- d+ 4
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- asym_sample[[d]][[t]]$X
    tv<- asym_sample[[d]][[t]]$tv_mi
    -mi_LNC(t(X)) - tv
  })
})

## knn-bw
source_python('lnn/lnn.py')

knn_bw<- sapply(1:16, function(d){
  D<- d+ 4
  print(d + 4)
  sapply(1:10, function(t){
    print(t)
    X<- asym_sample[[d]][[t]]$X
    tv<- asym_sample[[d]][[t]]$tv_mi
    -(entropy(X) - sum(sapply(1:D, function(d) entropy(X[,d, drop = F])))) - tv
  })
})

save(ec, classical_1, classical_5, classical_10, classical_15, classical_25, KSG, LNC, knn_bw, asym_sample, file = 'asym_sample_mi.RData')