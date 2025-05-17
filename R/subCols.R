#' @name subCols
#' @title Subsetting a "dist" object by columns
#'
#' @description  Efficiently extracts a column-wise subset of a "dist" object, returning the corresponding submatrix of pairwise distances. # nolint
#'
#' @usage subCols(dist, idx)
#'
#' @param dist A "dist" object, which can be computed via the stats::dist function, representing pairwise distances between observations.
#' @param idx An integer vector, specifying the column indices of the subsetted matrix.
#'
#' @details
#' This function extracts specified columns from a "dist" object without explicit conversion to a dense distance "matrix",
#' resulting in better performance and reduced memory overhead. Particularly useful when only a subset of distances is needed for downstream tasks.
#'
#' @return A numeric "matrix" containing the pairwise distances between all rows and the specified columns.
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
#' #Randomly subsetting a 50x10 matrix
#' idx = sample(1:50, 10)
#' microbenchmark(base::as.matrix(dx)[1:50,idx],
#'                proxy::as.matrix(dx)[1:50,idx],
#'                subCols(dx, idx))
#' #Check if equal
#' v1 = as.vector(base::as.matrix(dx)[1:50,idx])
#' v2 = as.vector(subCols(dx, idx))
#' all.equal(v1, v2)
#'
#' @author Minh Long Nguyen \email{edelweiss611428@gmail.com}
#' @export

subCols = function(dist, idx){

  checkDist(dist)
  N = attr(dist, "Size")
  idx = checkIdx(idx,N) #double to int conversion

  return(.subsetColsCpp(dist, idx - 1L))

}
