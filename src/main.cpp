#include<RcppArmadillo.h>
#include "distances.h"

// [[Rcpp::export]]
double DistMinkowski(const arma::vec& x,
                     const arma::vec& y,
                     int n){
  return minkowskiCpp(x,y, n);
};

// [[Rcpp::export]]
double DistMaximum(const arma::vec& x,
                     const arma::vec& y){
  return maximumCpp(x,y);
};

// [[Rcpp::export]]
double DistCanberra(const arma::vec& x,
                    const arma::vec& y){
  return canberraCpp(x,y);
};


// [[Rcpp::export]]
double DistCosine(const arma::vec& x,
                    const arma::vec& y){
  return cosineCpp(x,y);
};





