#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// non-pointer-based solution (deprecated)
// // get2dFrom1dCpp(): 1d to 2d index conversion
// // [[Rcpp::export]]
// IntegerMatrix get2dFrom1dCpp(const IntegerVector&idx, int n){
//
//   int nc = idx.size();
//   int n1 = n-1;
//   int N = n*(n-1)/2;
//
//   IntegerMatrix idx2d(2,nc);
//   int j = 0;
//   int c;
//   int a;
//
//   for(int i = 0; i < nc; i++){
//     a = 1+8*(N - idx[i]);
//     c = floor((-1 + std::sqrt(a))/2);
//     idx2d[j] = n1 - c;
//     idx2d[j+1] = n - (N-idx[i]) + c*(c+1)/2;
//     j += 2;
//   }
//
//   return idx2d;
//
// }


// get2dFrom1dCpp(): 1d to 2d index conversion
// [[Rcpp::export(.get2dFrom1dCpp)]]
IntegerMatrix get2dFrom1dCpp(const IntegerVector &idx, int n) {
  // Here idx >= 1 not 0 !!!
  int nc = idx.size();
  int n1 = n - 1;
  int N = n * (n - 1) / 2;

  IntegerMatrix idx2d(2, nc); //output idx counts from 1!!!

  // Raw pointers for access
  const int* idx_ptr = idx.begin();         // ptr to idx
  int* idx2dptr = INTEGER(idx2d);           // ptr to idx2d (output)
  int j = 0;

  for (int i = 0; i < nc; ++i) {
    int a = 1 + 8 * (N - idx_ptr[i]);
    int c = static_cast<int>(std::floor((-1 + std::sqrt(a)) / 2.0));
    idx2dptr[j++] = n1 - c;                                  // row idx
    idx2dptr[j++] = n - (N - idx_ptr[i]) + c * (c + 1) / 2;  // col idx
  }

  return idx2d;
}
