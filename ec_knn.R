## entropy estimation with local ellipse correction
library(FNN)
library(mvtnorm)

reference_correction<- function(D, k, radius){
  ## Sample from a uniform distribtionted ball
  ## Normal random vector approach
  r<- c(1, runif(k, 0, 1))^(1/D)
  # r<- runif((k+1), 0, 1)^(1/D)
  y<- rmvnorm(k + 1, rep(0, D), diag(D))
  U<- (r * y)/apply(y, 1, function(x) sqrt(sum(x^2))) * radius
  center.idx<- which.min(apply(U, 1, function(x)sqrt(sum(x^2))))
  
  # r<- c(1, runif((k-1), 0, 1))^(1/D)
  # # r<- runif((k+2), 0, 1)^(1/D)
  # y<- rmvnorm(k, rep(0, D), diag(D))
  # U<- (r * y)/apply(y, 1, function(x) sqrt(sum(x^2)))
  # # center.idx<- which.min(apply(U, 1, function(x)sqrt(sum(x^2))))
  # U<- rbind(0, U)
  # center.idx<- 1
  # U<- t(apply(U, 1, function(u) u + center))
  
  ## elliptical projetion
  ## e<- eigen(cov(neighborhood))
  e<- eigen(cov(U))
  neighbor_cord_new<- apply(e$vectors, 2, function(v) U %*% v)
  neighbor_new_center<- apply(neighbor_cord_new, 2, mean)
  # neighbor_new_center<- neighbor_cord_new[center.idx,]
  neighbor_cord_new_center_shift<- t(apply(neighbor_cord_new, 1, function(v) v - neighbor_new_center))
  axes<- apply(neighbor_cord_new_center_shift, 2, function(x) max(abs(x)))
  epsilon_new<- max(as.matrix(dist(neighbor_cord_new_center_shift))[center.idx,])
  
  ## correction
  sum(log(axes/epsilon_new))
  # sum(log(axes))
}

entropy_ec<- function(X, k, incr_ratio = .01, k_inner = k, k_x_th = 0, cl = NULL){
  N<- nrow(X)
  D<- ncol(X)
  epsilon<- knn.dist(X, k)[,k]
  epsilon_1<- knn.dist(X, k_inner)[,k_inner]
  neibour_ind<- knn.index(X, k=k)
  neibours<- lapply(1:N, function(n) X[neibour_ind[n,],])
  
  # e<- eigen(cov(X))
  correction<- sapply(1:N, function(n){
    x<- X[n,]
    neighborhood<- rbind(x, neibours[[n]])
    
    ## volumne correction
    e<- eigen(cov(neighborhood))
    neighbor_cord_new<- apply(e$vectors, 2, function(v) neighborhood %*% v)
    
    neighbor_new_center<- apply(neighbor_cord_new, 2, mean)
    # neighbor_new_center<- neighbor_cord_new[1,]
    neighbor_cord_new_center_shift<- t(apply(neighbor_cord_new, 1, function(v) v - neighbor_new_center))
    axes<- apply(neighbor_cord_new_center_shift, 2, function(x) max(abs(x)))
    epsilon_new<- max(as.matrix(dist(neighbor_cord_new_center_shift))[1,])
    empiri_volumne_correction<- sum(log(axes/epsilon_new))
    
    ## compare to reference correction
    ref_ellipsoid<- reference_correction(D, k, epsilon[n])
    if(empiri_volumne_correction < ref_ellipsoid){
      ## neighbor correction
      # cord_new<- apply(e$vectors, 2, function(v) X %*% v)
      # cord_new_center_shift<- t(apply(neighbor_cord_new, 1, function(v) v - neighbor_cord_new[1,]))
      # k_x<- length(which(apply(cord_new_center_shift[1:k,], 1, function(x) sum((x^2)/(axes^2)))<=1))
      # epsilon_new<- max(as.matrix(dist(neighbor_cord_new_center_shift))[1,])
      # axes<- (epsilon_new/max(axes)) * axes
      ratio = 1
      k_x<- length(which(apply(neighbor_cord_new_center_shift[2:k,], 1, function(x) sum((x^2)/(axes^2)))<=1))
      
      # recalculate neighbors inside of the ellipsoid
      while(k_x <= k_x_th){
        ratio<- ratio + incr_ratio
        axes<-  ratio * axes
        k_x<- length(which(apply(neighbor_cord_new_center_shift[2:k,], 1, function(x) sum((x^2)/(axes^2)))<=1))
      }
      
      # out<- -digamma(k_x) + empiri_volumne_correction
      out<- -digamma(k_x) + empiri_volumne_correction + D * log(ratio)
    }
    else{
      # out<- -digamma(k)
      out<- -digamma(k_inner)
    }
    
    out
  })
  
  digamma(N-1) + log((pi^(D/2))/gamma(1 + D/2)) + D*mean(log(epsilon_1)) + mean(correction, na.rm = T)
}