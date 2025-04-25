#' @name fastDistAB
#' @title Pairwise distances between rows of two matrices
#'
#' @description  This function efficiently computes pairwise distances between rows of two numeric matrices.
#'
#' @usage fastDistAB(A, B, method, p = 2)
#'
#' @param A A numeric matrix.
#' @param B A numeric matrix.
#' @param method A string specifying a distance method. Supported methods include "euclidean", "manhattan", "maximum", "minkowski", "cosine", and "canberra".
#' @param p A positive integer, required for computing Minkowski distance; by default p = 2 (i.e., Euclidean).
#'
#' @details This function computes a distance "matrix" storing pairwise distances between rows of two numeric matrices A and B.
#'
#' @importFrom microbenchmark microbenchmark
#' @importFrom proxy dist
#' @importFrom stats na.fail
#' @return A matrix storing pairwise distances between rows of A and B.
#'
#' @examples
#'
#' library("microbenchmark")
#' X = matrix(rnorm(200), nrow = 50)
#' A = X[1:25,]
#' B = X[26:50,]
#' microbenchmark(proxy::dist(A,B, "minkowski", p = 5),
#'                fastDistAB(A,B, "minkowski", p = 5L))
#' all.equal(proxy::dist(A,B, "minkowski", p = 5), fastDistAB(A,B, "minkowski", p = 5L))
#' @author Minh Long Nguyen \email{edelweiss611428@gmail.com}
#' @export


fastDistAB = function(A, B, method, p = 2L){

  if(!is.matrix(A) | !is.numeric(A)){
    stop("A must be a numeric matrix!")
  } else{
    na.fail(A)
  }

  if(!is.matrix(B) | !is.numeric(B)){
    stop("B must be a numeric matrix!")
  } else{
    na.fail(B)
  }

  if(length(method) != 1 |!is.character(method)){
    stop("Invalid method!")
  }

  if(length(p) != 1 |!is.numeric(p)){
    stop("Invalid p!")
  } else{
    p = as.integer(p)
    if(p < 0){
      stop("p >= 1 is required!")
    }
  }

  return(.fastDistABCpp(A, B, method, p))

}

