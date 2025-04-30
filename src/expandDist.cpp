#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;
// expandDistCpp(): Compute a pair-wise distance matrix of class "dist"
// [[Rcpp::export(.expandDistCpp)]]
NumericVector expandDistCpp(const NumericVector& distA, //dist obj
                             const NumericMatrix& distBA,
                             const NumericVector& distB, //dist obj
                             bool diag  = false, bool upper = false){
  //counts from 0
  const int nA = distA.attr("Size");
  const int nB = distB.attr("Size");
  int nr = nA +nB;
  if(nr > 65535){
    Rcpp::stop("The number of rows in the combined matrix should be less than 65535!");
  }

  long long tmp = static_cast<long long>(nr) * (nr - 1) >> 1;
  const int len = static_cast<int>(tmp);
  NumericVector out(len);

  const double* BAptr = &distBA(0, 0);    // column-major
  const double* Aptr = REAL(distA);    //
  const double* Bptr = REAL(distB);    //
  double*       optr = REAL(out);

  int idx = 0;
  int k = 0;
  int m = 0;

  for (int i = 0; i < nA; i++) { //i < nA instead of nA - 1 so that the //* loop wont be run at i = nA-1
    for (int j = i+1; j < nA; j++) { //*
      optr[idx++] = Aptr[k];
      k++;
    }
    for (int j = 0; j < nB; j++) {
      optr[idx++] = BAptr[m];
      m++;
    }
  }

  int lendB = distB.size();

  for(int j = 0; j < lendB; j++){
    optr[idx++] = Bptr[j];
  }

  // dist attrs
  out.attr("Size")  = nr;
  out.attr("Diag")  = diag;
  out.attr("Upper") = upper;
  out.attr("class") = "dist";

  return out;
}
