#include<RcppArmadillo.h>

using namespace Rcpp;


inline double euclideanCpp(const double* xi, const double* xj, int nr1, int nr2, int nc) {

  double sum = 0.0;
  double d;

  for (int k = 0; k < nc; k++, xi += nr1, xj += nr2) {
    d = *xi - *xj;
    sum += d * d;
  }

  return std::sqrt(sum);
};

inline double manhattanCpp(const double* xi, const double* xj, int nr1, int nr2, int nc) {

  double sum = 0.0;

  for (int k = 0; k < nc; k++, xi += nr1, xj += nr2) {
    sum += std::abs(*xi - *xj);
  }

  return sum;
};

inline double minkowskiCpp(const double* xi, const double* xj, int nr1, int nr2, int nc, int p) {

  if(p == 2){
    return euclideanCpp(xi, xj, nr1, nr2, nc);
  } else if (p == 1) {
    return manhattanCpp(xi, xj, nr1, nr2, nc);
  }

  double sum = 0.0;

  for (int k = 0; k < nc; k++, xi += nr1, xj += nr2) {
    sum += std::pow(std::abs(*xi - *xj), p);
  }

  return std::pow(sum, 1.0/p);

};

inline double chebyshevCpp(const double* xi, const double* xj, int nr1, int nr2, int nc) {

  double max_d = 0;

  for (int k = 0; k < nc; k++, xi += nr1, xj += nr2) {
    double d = std::abs(*xi - *xj);
    if (d > max_d) {
      max_d = d;
    }
  }
  return max_d;
};


inline double canberraCpp(const double* xi, const double* xj, int nr1, int nr2, int nc) {

  double d; //denominator
  double n; //numerator
  double sum = 0.0;

  for (int k = 0; k < nc; k++, xi += nr1, xj += nr2) {
    n = std::abs(*xi - *xj);
    d = std::abs(*xi)  + std::abs(*xj);
    if (d > 0) {
      sum += n/d;
    }
  }

  return sum;

};


inline double cosineCpp(const double* xi, const double* xj, int nr1, int nr2, int nc) {
  double dot_prod = 0.0; //dotproduct
  double nxi = 0.0; // norm(xi,2)
  double nxj = 0.0; // norm(xj,2)

  for (int k = 0; k < nc; k++, xi += nr1, xj += nr2) {
    dot_prod += *xi**xj;
    nxi += *xi**xi;
    nxj += *xj**xj;
  }

  return  1 - dot_prod/std::sqrt(nxi * nxj);
};



// [[Rcpp::export]]
NumericVector fastDistCpp(const NumericMatrix& X, std::string method = "euclidean",
                       bool diag  = false, bool upper = false, int p = 2){
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

    for (int i = 0; i < nr; i++) {
      xi = xptr + i;
      for (int j = i+1; j < nr; j++) {
        xj = xptr + j;
        // X(i,k) equiv to xptr[k*n + i],
        optr[idx++] = euclideanCpp(xi, xj, nr, nr, nc);
      }
    }

  } else if (method == "manhattan"){

    for (int i = 0; i < nr; i++) {
      xi = xptr + i;
      for (int j = i+1; j < nr; j++) {
        xj = xptr + j;
        // X(i,k) equiv to xptr[k*n + i],
        optr[idx++] = manhattanCpp(xi, xj, nr, nr, nc);
      }
    }

  } else if (method == "minkowski"){

    for (int i = 0; i < nr; i++) {
      xi = xptr + i;
      for (int j = i+1; j < nr; j++) {
        xj = xptr + j;
        // X(i,k) equiv to xptr[k*n + i],
        optr[idx++] = minkowskiCpp(xi, xj, nr, nr, nc, p);
      }
    }

  } else if (method == "maximum"){

    for (int i = 0; i < nr; i++) {
      xi = xptr + i;
      for (int j = i+1; j < nr; j++) {
        xj = xptr + j;
        // X(i,k) equiv to xptr[k*n + i],
        optr[idx++] = chebyshevCpp(xi, xj, nr, nr, nc);
      }
    }

  } else if (method == "canberra"){

    for (int i = 0; i < nr; i++) {
      xi = xptr + i;
      for (int j = i+1; j < nr; j++) {
        xj = xptr + j;
        // X(i,k) equiv to xptr[k*n + i],
        optr[idx++] = canberraCpp(xi, xj, nr, nr, nc);
      }
    }

  } else if (method == "cosine"){

    for (int i = 0; i < nr; i++) {
      xi = xptr + i;
      for (int j = i+1; j < nr; j++) {
        xj = xptr + j;
        // X(i,k) equiv to xptr[k*n + i],
        optr[idx++] = cosineCpp(xi, xj, nr, nr, nc);
      }
    }

  } else {
    Rcpp::stop("Method is not supported!");
  }

  // dist attrs
  out.attr("Size")  = nr;
  out.attr("Diag")  = diag;
  out.attr("Upper") = upper;
  out.attr("Method") = method;
  out.attr("class") = "dist";

  return out;
}


