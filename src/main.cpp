#include<RcppArmadillo.h>
#include "distances.h"

int indexing(int nr, int i, int j){

  if (i < j+1){
    int temp;
    temp = i;
    i = j;
    j = temp;
  } else if(i == j) {
    return 0;
  }

  return ((2*nr-1-j)*j >> 1) - 1 + (i-j);

}


// [[Rcpp::export]]
NumericVector dist(const arma::mat& X, bool diag = false, bool upper = false){

  int nr = X.n_rows;
  int len = nr*(nr-1) >> 1;
  NumericVector dmat(len);
  int id;

  for (int i = 0; i < nr; i++) {
    for (int j = i+1; j < nr; j++) {
      id = indexing(nr, i, j);
      dmat(id) = euclideanCpp(X.row(i), X.row(j));
    }
  }

  dmat.attr("Size") = nr;
  dmat.attr("Diag") = diag;
  dmat.attr("Upper") = upper;
  dmat.attr("class") = "dist";

  return dmat;



}
