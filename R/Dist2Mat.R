#' @name Dist2Mat
#' @title Dist2Mat conversion
#'
#' @description  This function efficiently converts a "dist" object to a distance "matrix".
#'
#' @usage Dist2Mat(dist)
#'
#' @param dist A "dist" object, which can be computed via the stats::dist() function.
#'
#' @details This function efficiently converts a "dist" object to a distance "matrix". It is faster than base::as.matrix() and proxy::as.matrix().
#'
#' @importFrom microbenchmark microbenchmark
#' @importFrom proxy as.matrix
#' @return A distance "matrix".
#'
#' @examples
#'
#' library("microbenchmark")
#' x = matrix(rnorm(200), nrow = 50)
#' dx = dist(x)
#' #Dist2Mat conversion
#' microbenchmark(base::as.matrix(dx),
#'                proxy::as.matrix(dx),
#'                Dist2Mat(dx))
#' #Check if equal
#' all.equal(base::as.matrix(dx), Dist2Mat(dx))
#'
#' @author Minh Long Nguyen \email{edelweiss611428@gmail.com}
#' @export


Dist2Mat = function(dist){

  if(!inherits(dist, "dist")) {
    stop("dist must be a 'dist' object!")
  }

  return(.Dist2MatCpp(dist))

}

