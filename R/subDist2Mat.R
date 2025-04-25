#' @name subDist2Mat
#' @title Dist2Mat subsetting
#'
#' @description  This function efficiently subsets a "dist" object to a smaller distance "matrix".
#'
#' @usage subDist2Mat(dist, idx1, idx2)
#'
#' @param dist A "dist" object, which can be computed via the stats::dist() function, representing the full pairwise distance matrix between observations.
#' @param idx1 An integer vector, specifying the row indices of the subsetted matrix.
#' @param idx2 An integer vector, specifying the column indices of the subsetted matrix.
#'
#' @details Lorem ipsum.
#'
#' @importFrom microbenchmark microbenchmark
#' @importFrom proxy as.matrix
#' @return A matrix storing pairwise distances between pairs in idx1 x idx2.
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
#'                subDist2Mat(dx, idx1, idx2),
#'                times = 100)
#' #Check if equal
#' all.equal(as.vector(base::as.matrix(dx)[idx1,idx2]), as.vector(subDist2Mat(dx, idx1, idx2)))
#'
#' @author Minh Long Nguyen \email{edelweiss611428@gmail.com}
#' @export

subDist2Mat = function(dist, idx1, idx2){

  if(!inherits(dist, "dist")) {
    stop("dist must be a 'dist' object!")
  }

  N = attr(dist, "Size")

  if(!is.numeric(idx1) | max(idx1) > N | min(idx1) < 1){
    stop("Invalid idx1!")
  }

  if(!is.numeric(idx2) | max(idx2) > N | min(idx2) < 1){
    stop("Invalid idx2!")
  }


  idx1 = as.integer(idx1)
  idx2 = as.integer(idx2)

  return(.subsetDist2MatCpp(dist, idx1-1L, idx2-1L))

}
