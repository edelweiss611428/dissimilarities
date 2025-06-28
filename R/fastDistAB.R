#' @name fastDistAB
#' @title Computing pairwise distances between rows of two matrices
#'
#' @description
#' Efficiently computes pairwise distances between the rows of two numeric matrices using various distance metrics.
#'
#' @usage fastDistAB(A, B, method = "euclidean", p = 2L)
#'
#' @param A A numeric matrix.
#' @param B A numeric matrix.
#' @param method A character string specifying the distance metric to use. Supported methods include
#' \code{"euclidean"}, \code{"manhattan"}, \code{"maximum"}, \code{"minkowski"}, \code{"cosine"}, and \code{"canberra"}.
#' @param p A positive integer, required for computing Minkowski distance; by default p = 2 (i.e., Euclidean).
#'
#' @details
#' This function computes the full pairwise distance matrix between the rows of matrices \code{A} and \code{B},
#' without forming a concatenated matrix or performing unnecessary intermediate conversions. It supports multiple
#' commonly used distance measures and is optimised for speed.
#'
#' Row names in A and B are retained. If either rownames(A) or rownames(B) is null, as.character(1:nrow(A)) and as.character(1:nrow(B))
#' will be used as row and column names of the resulting matrix instead.
#'
#' @return A numeric matrix of dimensions \code{nrow(A)} by \code{nrow(B)}, where each entry represents the distance between a row in \code{A} and a row in \code{B}.
#'
#' @importFrom microbenchmark microbenchmark
#' @importFrom proxy dist
#' @importFrom stats na.fail
#'
#' @examples
#'
#' library("microbenchmark")
#' X = matrix(rnorm(200), nrow = 50)
#' A = X[1:25,]
#' B = X[26:50,]
#' microbenchmark(proxy::dist(A,B, "minkowski", p = 5),
#'                fastDistAB(A,B, "minkowski", p = 5L))
#' #Check if equal
#' v1 = as.vector(proxy::dist(A,B, "minkowski", p = 5))
#' v2 = as.vector(fastDistAB(A,B, "minkowski", p = 5L))
#' all.equal(v1, v2)
#' @author Minh Long Nguyen \email{edelweiss611428@gmail.com}
#' @export


fastDistAB = function(A, B, method = "euclidean", p = 2L){

  checkMat(A)
  checkMat(B)
  checkMethod(method)
  p = checkP(p) #double to int conversion

  distAB =.fastDistABCpp(A, B, method, p)

  if(is.null(rownames(A)) | is.null(rownames(B))){
    rownames(distAB) = as.character(1:nrow(A))
    colnames(distAB) = as.character(1:nrow(B))
  } else{
    rownames(distAB) = rownames(A)
    colnames(distAB) = rownames(B)
  }

  return(distAB)

}

