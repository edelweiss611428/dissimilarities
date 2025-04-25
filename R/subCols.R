#' @name subCols
#' @title Subsetting by column from a "dist" object
#'
#' @description  This function efficiently subsets a "dist" object by column to a smaller distance "matrix".
#'
#' @usage subCols(dist, idx)
#'
#' @param dist A "dist" object, which can be computed via the stats::dist() function, representing the full pairwise distance matrix between observations.
#' @param idx An integer vector, specifying the column indices of the subsetted matrix.
#'
#' @details This function efficiently extracts columns from a "dist" object without involving unnecessary conversion (to a full data matrix), thereby improving efficiency and memory usage.
#'
#' @importFrom microbenchmark microbenchmark
#' @importFrom proxy as.matrix
#' @importFrom stats na.fail
#' @return A matrix storing pairwise distances between pairs in idx1 x idx2.
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
#' all.equal(as.vector(base::as.matrix(dx)[1:50,idx]), as.vector(subCols(dx, idx)))
#'
#' @author Minh Long Nguyen \email{edelweiss611428@gmail.com}
#' @export

subCols = function(dist, idx){

  if(!inherits(dist, "dist")) {
    stop("dist must be a 'dist' object!")
  }

  N = attr(dist, "Size")

  if(!is.numeric(idx)){
    stop("Numeric idx is required!")
  } else{
    na.fail(idx)
    idx = as.integer(idx)
    if(max(idx) > N | min(idx) < 1){
      stop("idx not in range [1,N]!")
    }
  }

  return(.subsetColsCpp(dist, idx - 1L))

}
