#' EMalorithm to estimation proportion of blood genes A,B,O
#' @description This function obtains EM estimates of proportion of blood genes A,B,O
#' @param nA: number of individuals whose blood type is A
#' @param nAB: number of individuals whose blood type is AB
#' @param nB: number of individuals whose blood type is B
#' @param nO: number of individuals whose blood type is O
#' @return EM estimates of proportion of blood genes A,B,O
#' @examples
#' nA,nAB,nB,nO = c(6,4,55,35)
#' p = EM_abo(6,4,55,35)
#' @export

EM_abo<-function(nA,nAB,nB,nO){
  n<-nA+nB+nAB+nO
  pA<-1/3;pB<-1/3;pO<-1/3
  delta<-1
  while(delta>=1e-5){
    # E-step
    mAA<-nA*pA^2/(pA^2+2*pA*pO)
    mAO<-nA*2*pA*pO/(pA^2+2*pA*pO)
    mBB<-nB*pB^2/(pB^2+2*pB*pO)
    mBO<-nB*2*pB*pO/(pB^2+2*pB*pO)
    # M_step
    pA1<-(2*mAA+mAO+nAB)/(2*n)
    pB1<-(2*mBB+mBO+nAB)/(2*n)
    pO1<-(2*nO+mBO+mAO)/(2*n)
    # delta:
    a<-c(pA1-pA,pB1-pB,pO1-pO)
    delta<-max(abs(a))
    pA<-pA1;pB<-pB1;pO<-pO1
    print(delta)
  }
  p<-matrix(c(pA,pB,pO),nrow = 1,ncol = 3)
  colnames(p)<-c("pA","pB","pO")
  return(p)
}

