#include<RcppArmadillo.h>
#include "distances.h"

double manhattanCpp(const arma::rowvec& x, const arma::rowvec& y){
  return arma::norm(x - y, 1);
};

double euclideanCpp(const arma::rowvec& x, const arma::rowvec& y){
  return arma::norm(x - y, 2);
};

double maximumCpp(const arma::rowvec& x, const arma::rowvec& y){
  return arma::norm(x - y, "inf");
};


double canberraCpp(const arma::rowvec& x, const arma::rowvec& y){
  double dist = 0.0;
  for (int i = 0; i < x.n_elem; i++) {
    double n = std::abs(x(i) - y(i));
    double d = std::abs(x(i)) + std::abs(y(i));
    if (d > 0) {
      dist += n/d;
    }
  }
  return dist;
};


double cosineCpp(const arma::rowvec& x, const arma::rowvec& y){
  return 1 - arma::dot(x, y)/(arma::norm(x, 2) * arma::norm(y, 2));
};




