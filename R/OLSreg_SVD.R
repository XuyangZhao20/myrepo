#' OLS estimates of the regression coefficients using a SVD decomposition
#' @description This function obtains OLS estimates of coefficients using a SVD decomposition
#' @param x: design matrix, each columns represents a predict variable
#' @param y: dependent variable
#' @return OLS estimates of coefficients in the regression model
#' @examples
#' x: a 10*3 matrix
#' y: a vertor with length of 10
#' OLSreg.SVD(x,y)
#' @export

OLSreg.SVD<-function(x,y){
  SVD<-svd(x)
  u<-SVD$u
  v<-SVD$v
  sigma<-diag(SVD$d)
  beta_hat<-v%*%solve(sigma)%*%t(u)%*%y
  beta_hat<-t(beta_hat)
  rownames(beta_hat)<-"beta.hat"
  colnames(beta_hat)<-c("x1","x2","x3","x4","x5")
  return(beta_hat)
}

