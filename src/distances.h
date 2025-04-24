#ifndef DISTANCES_H
#define DISTANCES_H

#include<RcppArmadillo.h>
using namespace Rcpp;

double manhattanCpp(const arma::vec& x, const arma::vec& y);

double euclideanCpp(const arma::rowvec& x, const arma::rowvec& y);

double maximumCpp(const arma::vec& x, const arma::vec& y);

double canberraCpp(const arma::vec& x, const arma::vec& y);

double cosineCpp(const arma::vec& x, const arma::vec& y);


#endif
