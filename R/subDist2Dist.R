#' @name subDist2Dist
#' @title Dist2Dist subsetting
#'
#' @description  Efficiently extracts a subset of observations from a "dist" object and returns a new "dist" object representing only the selected distances.
#'
#' @usage subDist2Dist(dist, idx, diag = FALSE, upper = FALSE)
#'
#' @param dist A "dist" object, which can be computed via the stats::dist function, representing the full pairwise distance matrix between observations.
#' @param idx An integer vector, specifying the indices of the observations to retain.
#' @param diag A boolean value, indicating whether to display the diagonal entries.
#' @param upper A boolean value, indicating whether to display the upper triangular entries.
#'
#' @details
#' This function subsets a "dist" object directly without explicit conversion to a dense distance "matrix".
#' It extracts only the relevant distances corresponding to the selected indices, improving both performance and memory efficiency.
#' The result is returned as a subsetted "dist" object, preserving compatibility with downstream functions that accept this class.
#'
#' @return A numeric "matrix" storing pairwise distances between the selected observations.
#'
#' @importFrom microbenchmark microbenchmark
#' @importFrom proxy as.matrix
#' @importFrom stats na.fail
#'
#' @examples
#'
#' library("microbenchmark")
#' x = matrix(rnorm(200), nrow = 50)
#' dx = dist(x)
#' #Subsetting the first 10 units
#' microbenchmark(as.dist(base::as.matrix(dx)[1:10,1:10]),
#'                as.dist(proxy::as.matrix(dx)[1:10,1:10]),
#'                subDist2Dist(dx, 1:10))
#' #Check if equal
#' v1 = as.vector(as.dist(base::as.matrix(dx)[1:10,1:10]))
#' v2 = as.vector(subDist2Dist(dx, 1:10))
#' all.equal(v1, v2)
#'
#' @author Minh Long Nguyen \email{edelweiss611428@gmail.com}
#' @export

subDist2Dist = function(dist, idx, diag = FALSE, upper = FALSE){

  checkDist(dist)
  N = attr(dist, "Size")
  checkBool(diag)
  checkBool(upper)
  idx = checkIdx(idx, N) #double to int conversion

  return(.subsetDist2DistCpp(dist, idx - 1L, diag, upper))

}
