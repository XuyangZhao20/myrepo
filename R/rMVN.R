#' Generate multivariate normal data
#' @description This function generates data that follow multivariate normal distribution
#' @param n: sample size
#' @param miu: the mean vector of multivariate normal distribution
#' @param sigma: the covariance matrix of multivariate distribution
#' @import MASS
#' @return data that follows MVN, and each row represents an observation
#' @examples
#' n: 5
#' m: c(2,1,2)
#' v: diag(c(1,2,3))
#' rMVN(n,m,v)
#' @export

rMVN<-function(n,miu,sigma){
  n1<-length(miu)
  L<-chol(sigma)
  I<-diag(rep(1,n1))
  miu0<-rep(0,n1)
  z<-mvrnorm(n,miu0,I)
  x<-matrix(nrow = n,ncol = 4)
  for(i in 1:n){
    x[i,]<-miu+L%*%z[i,]
  }
  return(x)
}
