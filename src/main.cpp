#include<RcppArmadillo.h>
#include "distances.h"

using namespace Rcpp;


inline double euclideanCpp(const double* xi, const double* xj, int nr, int nc) {

  double sum = 0.0;
  double d;

  for (int k = 0; k < nc; k++, xi += nr, xj += nr) {
    d = *xi - *xj;
    sum += d * d;
  }

  return std::sqrt(sum);
};

inline double manhattanCpp(const double* xi, const double* xj, int nr, int nc) {

  double sum = 0.0;

  for (int k = 0; k < nc; k++, xi += nr, xj += nr) {
    sum += std::abs(*xi - *xj);
  }

  return std::sqrt(sum);
};

inline double minkowskiCpp(const double* xi, const double* xj, int nr, int nc, int p) {

  if(p == 2){
    return euclideanCpp(xi, xj, nr, nc);
  } else if (p == 1) {
    return manhattanCpp(xi, xj, nr, nc);
  }

  double sum = 0.0;

  for (int k = 0; k < nc; k++, xi += nr, xj += nr) {
    sum += std::pow(std::abs(*xi - *xj), p);
  }

  return std::pow(sum, 1.0/p);
};



// [[Rcpp::export]]
NumericVector fastDist(const NumericMatrix& X, std::string method = "euclidean",
                       bool diag  = false, bool upper = false, int p = 2)
{
  const int nr   = X.nrow();
  const int nc   = X.ncol();
  const int len = nr * (nr - 1) / 2;
  NumericVector out(len);

  const double* xptr = REAL(X);    // column-major
  double*       optr = REAL(out);
  const double* xi;
  const double* xj;

  int idx = 0;

  if(method == "euclidean"){

    for (int i = 0; i < nr; ++i) {
      xi = xptr + i;
      for (int j = i+1; j < nr; ++j) {
        xj = xptr + j;
        // X(i,k) equiv to xptr[k*n + i],
        optr[idx++] = euclideanCpp(xi, xj, nr, nc);
      }
    }

  } else if (method == "manhattan"){

    for (int i = 0; i < nr; ++i) {
      xi = xptr + i;
      for (int j = i+1; j < nr; ++j) {
        xj = xptr + j;
        // X(i,k) equiv to xptr[k*n + i],
        optr[idx++] = manhattanCpp(xi, xj, nr, nc);
      }
    }

  } else if (method == "minkowski"){

    for (int i = 0; i < nr; ++i) {
      xi = xptr + i;
      for (int j = i+1; j < nr; ++j) {
        xj = xptr + j;
        // X(i,k) equiv to xptr[k*n + i],
        optr[idx++] = minkowskiCpp(xi, xj, nr, nc, p);
      }
    }

  }

  // dist attrs
  out.attr("Size")  = nr;
  out.attr("Diag")  = diag;
  out.attr("Upper") = upper;
  out.attr("class") = "dist";

  return out;
}


