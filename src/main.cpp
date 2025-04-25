#include<RcppArmadillo.h>
#include "distances.h"

using namespace Rcpp;

double euclideanCpp(const double* xi, const double* xj, int nr, int nc) {

  double sumsq = 0.0;
  double d;

  for (int k = 0; k < nc; k++, xi += nr, xj += nr) {
    d = *xi - *xj;
    sumsq += d * d;
  }

  return std::sqrt(sumsq);
};

double minkowskiCpp(const double* xi, const double* xj, int nr, int nc, int p) {

  double sumsq = 0.0;
  double d;

  for (int k = 0; k < nc; k++, xi += nr, xj += nr) {
    d = *xi - *xj;
    sumsq += std::pow(std::abs(d), p);
  }

  return std::sqrt(sumsq);
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

  std::function<double(const double*, const double*)> distfn;
  int idx = 0;

  if (method == "euclidean") {
    distfn = [nr, nc](const double* xi, const double* xj){
      return euclideanCpp(xi, xj, nr, nc);
      };
  } else if (method == "minkowski"){
    distfn = [nr, nc, p](const double* xi, const double* xj){
      return minkowskiCpp(xi, xj, nr, nc, p);
      };

    };

  for (int i = 0; i < nr; ++i) {
    xi = xptr + i;
    for (int j = i+1; j < nr; ++j) {
      xj = xptr + j;
      // X(i,k) equiv to xptr[k*n + i],
      optr[idx++] = distfn(xi, xj);
      }
    }

  // dist attrs
  out.attr("Size")  = nr;
  out.attr("Diag")  = diag;
  out.attr("Upper") = upper;
  out.attr("class") = "dist";

  return out;
}


