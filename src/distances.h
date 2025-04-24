#ifndef DISTANCES_H
#define DISTANCES_H

#include<RcppArmadillo.h>
using namespace Rcpp;

// Euclidean

double minkowskiCpp(const arma::vec& x,
                    const arma::vec& y,
                    int n);

double canberraCpp(const arma::vec& x,
                    const arma::vec& y);


#endif
