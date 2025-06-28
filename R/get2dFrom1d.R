#' @name get2dFrom1d
#' @title 1D-indexing to 2D-indexing
#'
#' @description  Efficiently computes 2D-indexing from 1D-indexing
#'
#' @usage get2dFrom1d(idx1d, N)
#'
#' @param idx1d An integer vector of 1D indexes
#' @param N The number of observations in the original data matrix
#'
#' @details
#' Converts 1D indexing (as used in R's "dist" objects) into 2D indexing (row-column pairs)
#' for a distance matrix of size \eqn{N \times N}.
#'
#' Currently, name-based indexing is not supported."
#'
#' @return An integer matrix storing the corresponding 2D indexes.
#'
#' @examples
#'
#' get2dFrom1d(1:10, 5)
#'
#' @author Minh Long Nguyen \email{edelweiss611428@gmail.com}
#' @export


get2dFrom1d = function(idx1d, N){

  checkP(N)
  checkIdx(idx1d, N*(N-1)/2)

  return(.get2dFrom1dCpp(idx1d, N))

}
