#' @name fastDist
#' @title `"dist"` object computation
#'
#' @description  Efficiently computes a `"dist"` object from a numeric matrix using various distance metrics.
#'
#' @usage fastDist(X, method = "euclidean", diag = F, upper = F, p = 2L)
#'
#' @param X A numeric matrix.
#' @param method A character string specifying the distance metric to use. Supported methods include
#' \code{"euclidean"}, \code{"manhattan"}, \code{"maximum"}, \code{"minkowski"}, \code{"cosine"}, and \code{"canberra"}.
#' @param diag A boolean value, indicating whether to display the diagonal entries.
#' @param upper A boolean value, indicating whether to display the upper triangular entries.
#' @param p A positive integer, required for computing Minkowski distance; by default p = 2 (i.e., Euclidean).
#'
#' @details
#' Calculates pairwise distances between rows of a numeric matrix and returns the result as a compact `"dist"` object, which stores
#' the lower-triangular entries of a complete distance matrix. Supports multiple distance measures, including `"euclidean"`,
#' `"manhattan"`, `"maximum"`, `"minkowski"`, `"cosine"`, and `"canberra"`.
#'
#' This implementation is optimised for speed, especially on large matrices.
#'
#' @importFrom microbenchmark microbenchmark
#' @importFrom proxy dist
#' @importFrom stats na.fail
#' @return A distance matrix of class `"dist"`.
#'
#' @examples
#'
#' library("microbenchmark")
#' x = matrix(rnorm(200), nrow = 50)
#' microbenchmark(stats::dist(x, "minkowski", p = 5),
#'                fastDist(x, "minkowski", p = 5))
#' v1 = as.vector(stats::dist(x, "minkowski", p = 5))
#' v2 = as.vector(fastDist(x, "minkowski", p = 5))
#' all.equal(v1, v2)
#'
#' @author Minh-Long Nguyen \email{edelweiss611428@gmail.com}
#' @export


fastDist = function(X, method = "euclidean", diag = F, upper = F, p = 2L){

  if(length(diag) != 1 | length(upper) != 1 |
     !is.logical(diag) | !is.logical(upper)){
    stop("Invalid (diag, upper) option!")
  }

  if(!is.matrix(X) | !is.numeric(X)){
    stop("X must be a numeric matrix!")
  } else{
    na.fail(X)
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

