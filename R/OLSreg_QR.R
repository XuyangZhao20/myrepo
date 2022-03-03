#' OLS estimates of the regression coefficients using a QR decomposition
#' @description This function obtains OLS estimates of coefficients using a QR decomposition
#' @param x: design matrix, each columns represents a predict variable
#' @param y: dependent variable
#' @return OLS estimates of coefficients in the regression model
#' @examples
#' x: a 10*3 matrix
#' y: a vertor with length of 10
#' OLSreg.QR(x,y)
#' @export

OLSreg.QR<-function(x,y){
  QR<-qr(x)
  Q<-qr.Q(QR)
  R<-qr.R(QR)
  beta_hat<-solve(R)%*%t(Q)%*%as.matrix(y)
  beta_hat<-t(beta_hat)
  rownames(beta_hat)<-"beta_hat"
  return(beta_hat)
}



