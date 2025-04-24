#include<RcppArmadillo.h>
#include "distances.h"

double minkowskiCpp(const arma::vec& x,
                    const arma::vec& y,
                    int n){
  return arma::norm(x - y, n);
};

double canberraCpp(const arma::vec& x,
                    const arma::vec& y){
  double dist = 0.0;
  for (int i = 0; i < x.n_elem; i++) {
    double n = std::abs(x(i) - x(i));
    double d = std::abs(x(i)) + std::abs(y(i));
    if (d > 0) {
      dist += n/d;
    }
  }
  return dist;
};




