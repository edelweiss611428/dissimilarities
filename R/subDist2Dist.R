#' @name subDist2Dist
#' @title Dist2Dist subsetting
#'
#' @description  This function efficiently subsets a "dist" object to a smaller "dist" object.
#'
#' @usage subDist2Dist(dist, idx, diag = F, upper = F)
#'
#' @param dist A "dist" object, which can be computed via the stats::dist() function, representing the full pairwise distance matrix between observations.
#' @param idx An integer vector, specifying the indices of the observations to retain.
#' @param diag A boolean value, indicating whether to display the diagonal entries.
#' @param upper A boolean value, indicating whether to display the upper triangular entries.
#'
#' @details Lorem ipsum.
#'
#' @importFrom microbenchmark microbenchmark
#' @importFrom proxy as.matrix
#' @return A subsetted "dist" object.
#'
#' @examples
#'
#' library("microbenchmark")
#' x = matrix(rnorm(200), nrow = 50)
#' dx = dist(x)
#' #Subsetting the first 10 units
#' microbenchmark(as.dist(base::as.matrix(dx)[1:10,1:10]),
#'                as.dist(proxy::as.matrix(dx)[1:10,1:10]),
#'                subDist2Dist(dx, 1:10),
#'                times = 100)
#' #Check if equal
#' all.equal(as.vector(as.dist(base::as.matrix(dx)[1:10,1:10])), as.vector(subDist2Dist(dx, 1:10)))
#'
#' @author Minh Long Nguyen \email{edelweiss611428@gmail.com}
#' @export

subDist2Dist = function(dist, idx, diag = F, upper = F){

  if(!inherits(dist, "dist")) {
    stop("dist must be a 'dist' object!")
  }

  N = attr(dist, "Size")

  if(length(diag) != 1 | length(upper) != 1 |
     !is.logical(diag) | !is.logical(upper)){
    stop("Invalid (diag, upper) option!")
  }

  if(!is.numeric(idx)){
    stop("Numeric idx is required!")
  } else{
    idx = as.integer(idx)
    if(max(idx) > N | min(idx) < 1){
      stop("idx not in range [1,N]!")
    }
  }

  return(.subsetDist2DistCpp(dist, idx - 1L, diag, upper))

}
