#' @name subDist2Mat
#' @title Dist2Mat subsetting
#'
#' @description  Efficiently extracts a 2d submatrix of pairwise distances from a `"dist"` object.
#'
#' @usage subDist2Mat(dist, idx1, idx2)
#'
#' @param dist A `"dist"` object, which can be computed via the [stats::dist] function, representing the full pairwise distance matrix between observations.
#' @param idx1 An integer vector, specifying the row indices of the subsetted matrix.
#' @param idx2 An integer vector, specifying the column indices of the subsetted matrix.
#'
#' @details
#' This function efficiently subsets a `"dist"` object by row and column indices, returning the corresponding rectangular section as a numeric matrix.
#' It avoids explicit conversion from the `"dist"` object to a dense `"matrix"`, improving memory efficiency and computational speed, especially with large datasets.
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

  if(!inherits(dist, "dist")) {
    stop("dist must be a 'dist' object!")
  }

  N = attr(dist, "Size")

  if(!is.numeric(idx1)){
    stop("Numeric idx1 is required!")
  } else{
    idx1 = as.integer(idx1)
    if(max(idx1) > N | min(idx1) < 1){
      na.fail(idx1)
      stop("idx1 not in range [1,N]")
    }
  }

  if(!is.numeric(idx2)){
    stop("Numeric idx2 is required!")
  } else{
    na.fail(idx2)
    idx2 = as.integer(idx2)
    if(max(idx2) > N | min(idx2) < 1){
      stop("idx2 not in range [1,N]")
    }
  }

  return(.subsetDist2MatCpp(dist, idx1-1L, idx2-1L))

}
