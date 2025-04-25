#' @name Dist2Mat
#' @title Dist2Mat conversion
#'
#' @description  This function efficiently converts a "dist" object to a distance "matrix".
#'
#' @usage Dist2Mat(dist)
#'
#' @param dist A "dist" object, which can be computed via the stats::dist() function.
#'
#' @details Lorem ipsum.
#'
#' @importFrom microbenchmark microbenchmark
#' @return a distance "matrix"
#'
#' @examples
#' library("microbenchmark")
#' x = matrix(rnorm(200), nrow = 50)
#' dx = dist(x)
#' #Dist2Mat conversion
#' microbenchmark(as.matrix(dx),
#'                Dist2Mat(dx),
#'                times = 100)
#'
#'
#' @author Minh Long Nguyen \email{edelweiss611428@gmail.com}
#' @export


Dist2Mat = function(dist){

  if(!inherits(dist, "dist")) {
    stop("dist must be a 'dist' object!")
  }

  return(.Dist2MatCpp(dist))

}

