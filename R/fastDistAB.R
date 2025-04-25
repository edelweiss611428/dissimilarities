#' @name fastDistAB
#' @title Pairwise distances between rows of two matrices
#'
#' @description  This function efficiently computes pairwise distances between rows of two numeric matrices.
#'
#' @usage fastDistAB(A, B, p = 2)
#'
#' @param A A numeric matrix.
#' @param B A numeric matrix.
#' @param method A string specifying a distance method. Supported methods include "euclidean", "manhattan", "maximum", "minkowski", "cosine", and "canberra".
#' @param p A positive interger, required for computing Minkowski distance; by default p = 2 (i.e., Euclidean).
#'
#' @details Lorem ipsum.
#'
#' @importFrom microbenchmark microbenchmark
#' @importFrom proxy dist
#' @return A matrix storing pairwise distances between rows of A and B.
#'
#' @examples
#'
#' library("microbenchmark")
#' A = matrix(rnorm(200), nrow = 50)
#' B = matrix(rnorm(200), nrow = 50)
#' microbenchmark(proxy::dist(A,B, method = "minkowski", p = 5),
#'                fastDistAB(A,B, method = "minkowski", p = 5))
#'
#' @author Minh Long Nguyen \email{edelweiss611428@gmail.com}
#' @export


fastDistAB = function(A, B, method, p = 2L){

  if(!is.matrix(A) | !is.numeric(A)){
    stop("A must be a numeric matrix!")
  } else{
    if(is.na(A)){
      stop("A contains NA elements!")
    }
  }

  if(!is.matrix(B) | !is.numeric(B)){
    stop("B must be a numeric matrix!")
  } else{
    if(is.na(B)){
      stop("B contains NA elements!")
    }
  }

  if(length(method) != 1 |!is.character(method)){
    stop("Invalid method!")
  }

  if(length(p) != 1 |!is.numeric(p)){
    stop("Invalid p!")
  } else{
    p = as.integer(0)
    if(p < 0){
      stop("p >= 1 is required!")
    }
  }

  return(fastDistABCpp(A, B, method, p))

}

