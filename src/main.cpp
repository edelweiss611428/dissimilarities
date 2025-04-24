#include<RcppArmadillo.h>
#include "distances.h"

// [[Rcpp::export]]
double DistMinkowski(const arma::vec& x,
                     const arma::vec& y,
                     int n){
  return minkowskiCpp(x,y, n);
};

// [[Rcpp::export]]
double DistCanberra(const arma::vec& x,
                    const arma::vec& y){
  return canberraCpp(x,y);
};




