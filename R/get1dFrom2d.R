#' @name get1dFrom2d
#' @title 2D-indexing to 1D-indexing
#'
#' @description  Efficiently computes 1D-indexing from 2D-indexing
#'
#' @usage get1dFrom2d(i,j, N)
#'
#' @param i An integer specifying the row index
#' @param j An integer specifying the column index - must be different from i as "dist" object does not store the diagonal entries.
#' @param N The number of observations in the original data matrix
#'
#' @details
#' Converts 2D indexing (a row-column pair) into 1D indexing (as used in R's "dist" objects), given the number
#' of observations N.
#'
#' @return An integer specifying the 1d index
#'
#' @examples
#' N = 5
#' for(i in 1:4){ 
#'   for(j in (i+1):5){
#'     print(get1dFrom2d(i,j,N))
#'   }
#' }
#'
#' @author Minh Long Nguyen \email{edelweiss611428@gmail.com}
#' @export


get1dFrom2d = function(i,j, N){

  checkP(N)
  checkP(i)
  checkP(j)
  if(i == j){
    stop("i must not be equal to j!")
  } else if(max(i,j) > N){
    stop("i,j must be smaller than or equal to N!")
  }

  return(.get1dFrom2dCpp(N, i-1, j-1)+1)

}

