#' @name Dist2Mat
#' @title Dist2Mat conversion
#'
#' @description  Efficiently converts a "dist" object into a symmetric distance "matrix".
#'
#' @usage Dist2Mat(dist)
#'
#' @param dist A "dist" object, which can be computed via the stats::dist function, representing pairwise distances between observations.
#'
#' @details Converts a "dist" object, typically created using the stats::dist function, into a symmetric matrix form.
#' This implementation is optimised for speed and performs significantly faster than base::as.matrix or proxy::as.matrix
#' when applied to "dist" objects.
#'
#' Row names are retained. If it is null, as.character(1:nObs) will be used as the row and column names of the resulting
#' matrix instead.
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
#' v1 = as.vector(base::as.matrix(dx))
#' v2 = as.vector(Dist2Mat(dx))
#' all.equal(v1, v2)
#'
#' @author Minh Long Nguyen \email{edelweiss611428@gmail.com}
#' @export


Dist2Mat = function(dist){

  checkDist(dist)

  distMat = .Dist2MatCpp(dist)
  rN = attr(dist, "Labels")

  if(is.null(rN)){
     rownames(distMat) = colnames(distMat) = as.character(1:attr(dist, "Size"))
  } else {
     rownames(distMat) = colnames(distMat) = rN
  }

  return(distMat)

}

