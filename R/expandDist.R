#' @name expandDist
#' @title Expanding a distance matrix given new data
#'
#' @description
#' Efficiently appends new "rows" to an existing "dist" object without explicitly recomputing a full pairwise distance matrix.
#'
#' @usage expandDist(distA, A, B, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
#'
#' @param distA A "dist" object, representing the pairwise distance matrix between observations in matrix A, ideally computed via
#' the distance metric specified in this function. This requires manual check.
#' @param A A numeric matrix.
#' @param B A numeric matrix.
#' @param method A character string specifying the distance metric to use. Supported methods include
#' \code{"euclidean"}, \code{"manhattan"}, \code{"maximum"}, \code{"minkowski"}, \code{"cosine"}, and \code{"canberra"}.
#' @param diag A boolean value, indicating whether to display the diagonal entries.
#' @param upper A boolean value, indicating whether to display the upper triangular entries.
#' @param p A positive integer, required for computing Minkowski distance; by default p = 2 (i.e., Euclidean).
#'
#' @details
#' Expands an existing distance matrix of class "dist" for matrix A, given new data B,
#' without explicitly computing the distance matrix of rbind(A,B).
#' This supports multiple commonly used distance measures and is optimised for speed.
#'
#' @return A distance matrix of class "dist" for rbind(A,B).
#'
#' @importFrom microbenchmark microbenchmark
#' @importFrom stats na.fail
#'
#' @examples
#'
#' A = matrix(rnorm(100), nrow = 20)
#' B = matrix(rnorm(250), nrow = 50)
#' AB = rbind(A,B)
#' distA = fastDist(A)
#' v1 = as.vector(expandDist(distA, A, B))
#' v2 = as.vector(fastDist(AB))
#' all.equal(v1, v2)
#' @author Minh Long Nguyen \email{edelweiss611428@gmail.com}
#' @export


expandDist = function(distA, A, B, method = "euclidean",
                      diag = FALSE, upper = FALSE, p = 2L){

  checkDist(distA)
  if(attr(distA, "Size") != nrow(A)){
    stop("The number of row in distA does not match that of A!")
  }

  checkMat(A)
  checkMat(B)
  p = checkP(p)
  checkMethod(method)
  distBA = .fastDistABCpp(B,A, method, p)
  distB = .fastDistCpp(B, method, diag, upper, p)

  return(.expandDistCpp(distA, distBA, distB, diag, upper))

}


