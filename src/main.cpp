#include<RcppArmadillo.h>
#include "distances.h"

// [[Rcpp::export]]
NumericVector dist(const arma::mat& X, bool diag = false, bool upper = false){

  int nr = X.n_rows;
  int len = nr*(nr-1) >> 1;
  NumericVector dmat(len);
  int id = 0;

  for (int i = 0; i < nr; i++) {
    for (int j = i+1; j < nr; j++) {
      dmat(id) = euclideanCpp(X.row(i), X.row(j));
      id++;
    }
  }

  dmat.attr("Size") = nr;
  dmat.attr("Diag") = diag;
  dmat.attr("Upper") = upper;
  dmat.attr("class") = "dist";

  return dmat;

}



