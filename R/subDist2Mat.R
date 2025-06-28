#' @name subDist2Mat
#' @title Dist2Mat subsetting
#'
#' @description  Efficiently extracts a 2d submatrix of pairwise distances from a "dist" object.
#'
#' @usage subDist2Mat(dist, idx1, idx2)
#'
#' @param dist A "dist" object, which can be computed via the stats::dist function, representing the full pairwise distance matrix between observations.
#' @param idx1 An integer vector, specifying the row indices of the subsetted matrix.
#' @param idx2 An integer vector, specifying the column indices of the subsetted matrix.
#'
#' @details
#' This function efficiently subsets a "dist" object by row and column indices, returning the corresponding rectangular section as a numeric matrix.
#' It avoids explicit conversion from the "dist" object to a dense "matrix", improving memory efficiency and computational speed, especially with large datasets.
#'
#' Row names are retained. If it is null, as.character(idx1) and as.character(idx2)
#' will be used as row and column names of the resulting matrix instead.
#'
#' @return A numeric matrix storing pairwise distances between observations column-indexed by \code{idx1} and row-indexed by \code{idx2}.
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
#' #Randomly subsetting a 10x10 matrix
#' idx1 = sample(1:50, 10)
#' idx2 = sample(1:50, 10)
#' microbenchmark(base::as.matrix(dx)[idx1,idx2],
#'                proxy::as.matrix(dx)[idx1,idx2],
#'                subDist2Mat(dx, idx1, idx2))
#' #Check if equal
#' v1 = as.vector(base::as.matrix(dx)[idx1,idx2])
#' v2 = as.vector(subDist2Mat(dx, idx1, idx2))
#' all.equal(v1, v2)
#'
#' @author Minh Long Nguyen \email{edelweiss611428@gmail.com}
#' @export

subDist2Mat = function(dist, idx1, idx2){

  checkDist(dist)
  N = attr(dist, "Size")
  idx1 = checkIdx(idx1, N) #double to int conversion
  idx2 = checkIdx(idx2, N)

  subDistMat = .subsetDist2MatCpp(dist, idx1-1L, idx2-1L)
  rN = attr(dist, "Labels")

  if(is.null(rN)){
    rownames(subDistMat) = as.character(idx1)
    colnames(subDistMat) = as.character(idx2)
  } else {
    rownames(subDistMat) =  rN[idx1]
    colnames(subDistMat) =  rN[idx2]
  }

  return(subDistMat)

}
