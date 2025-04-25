#' @name fastDist
#' @title "dist" object computation
#'
#' @description  This function efficiently computes a "dist" object from a numeric matrix.
#'
#' @usage fastDist(X, method = "euclidean", diag = F, upper = F, p = 2L)
#'
#' @param X A numeric matrix.
#' @param method A string specifying a distance method. Supported methods include "euclidean", "manhattan", "maximum", "minkowski", "cosine", and "canberra".
#' @param diag A boolean value, indicating whether to display the diagonal entries.
#' @param upper A boolean value, indicating whether to display the upper triangular entries.
#' @param p A positive interger, required for computing Minkowski distance; by default p = 2 (i.e., Euclidean).
#'
#' @details This function computes a distance matrix of class "dist", consisting of pair-wise distances between rows in the input data matrix. This object is internally an 1d array.
#'
#' @importFrom microbenchmark microbenchmark
#' @importFrom proxy dist
#' @return A distance matrix of class "dist".
#'
#' @examples
#'
#' library("microbenchmark")
#' x = matrix(rnorm(200), nrow = 50)
#' microbenchmark(stats::dist(x, method = "minkowski", p = 5),
#'                fastDist(x, method = "minkowski", p = 5))
#' all.equal(stats::dist(x, method = "minkowski", p = 5), fastDist(x, method = "minkowski", p = 5))
#'
#' @author Minh Long Nguyen \email{edelweiss611428@gmail.com}
#' @export


fastDist = function(X, method = "euclidean", diag = F, upper = F, p = 2L){

  if(length(diag) != 1 | length(upper) != 1 |
     !is.logical(diag) | !is.logical(upper)){
    stop("Invalid (diag, upper) option!")
  }

  if(!is.matrix(X) | !is.numeric(X)){
    stop("X must be a numeric matrix!")
  } else{
    if(is.na(X)){
      stop("X contains NA elements!")
    }
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

  return(.fastDistCpp(X, method, diag, upper, p))






}

