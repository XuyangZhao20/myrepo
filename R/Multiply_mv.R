#' Multiplication of two square matrices and a vector
#' @description This function performs multiplication of two square matrices and a vector
#' @param A: a square matrix
#' @param B: a square matrix
#' @param x: a vector
#' @param vector_first: a parameter which decides the order of multiplication. E.g. If vector_first = True, the function will compute the product of a matrix and a vector. Else, it will first compute the product of two matrices.
#' @return a product
#' @examples
#' A, B: 3*3 matrices
#' x: a vector with length of 3
#' Multiply_mv(A,B,x, vector_first = T)
#' @export

Multiply_mv<-function(A,B,x,vector_first){
  if(vector_first == T){
    temp <- B%*%x
    result <- A%*%temp
  }else{
    temp <- A%*%B
    result <- temp%*%x
  }
  return(result)
}

