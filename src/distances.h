#ifndef DISTANCES_H
#define DISTANCES_H

#include<RcppArmadillo.h>
using namespace Rcpp;

double minkowskiCpp(const arma::vec& x, const arma::vec& y, int n);

double maximumCpp(const arma::vec& x, const arma::vec& y);

double canberraCpp(const arma::vec& x, const arma::vec& y);

double cosineCpp(const arma::vec& x, const arma::vec& y);


#endif
