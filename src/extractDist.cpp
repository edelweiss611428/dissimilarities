#include <Rcpp.h>
using namespace Rcpp;

// indexing(): Convert a 2d index to an 1d index
inline int indexing(int nr, int i, int j){

  //here i and j count from 0

  // deprecated
  // if(i == j){
  //   return NumericVector::get_na();
  // }
  // if (i < j+1){
  //   int temp;
  //   temp = i;
  //   i = j;
  //   j = temp;
  // }

  if (i < j+1) std::swap(i, j);

  // return ((2*nr-1-j)*j >> 1) - 1 +(i-j);

  return (nr*j - j*(j+1)/2 + (i-j-1)); //output idx counts from 0

}

// subsetDist2DistCpp(): Extracting a "dist" object corresponding to DistanceMatrix[idx,idx].
// [[Rcpp::export(.subsetDist2DistCpp)]]
NumericVector subsetDist2DistCpp(const NumericVector &dist, const IntegerVector & idx,
                                 bool diag = false, bool upper = false){

 //here idx counts from 0

  int N = dist.attr("Size");
  int n = idx.size();

  if(n > 65535){
    Rcpp::stop("length(idx) exceeds 65535!");
  }
  long long tmp = static_cast<long long>(n) * (n - 1) >> 1;
  const int len = static_cast<int>(tmp);
  NumericVector subdmat(len);

  double* subdmatptr = REAL(subdmat);
  double* distptr = REAL(dist);
  const int* idxptr = idx.begin();
  int k = 0;

  for(int i = 0; i < n-1; i++){
    for(int j = (i+1); j < n; j++){
      subdmatptr[k++] = distptr[indexing(N, idxptr[i], idxptr[j])];
    }
  }

  subdmat.attr("Size") = n;
  subdmat.attr("Diag") = diag;
  subdmat.attr("Upper") = upper;
  subdmat.attr("class") = "dist";

  return subdmat;

}


// subsetDist2MatCpp(): Extracting a distance "matrix" object corresponding to DistanceMatrix[idx1,idx2]
// [[Rcpp::export(.subsetDist2MatCpp)]]
NumericMatrix subsetDist2MatCpp(const NumericVector &dist, const IntegerVector &idx1, const IntegerVector &idx2){

  int N = dist.attr("Size");
  int n1 = idx1.size();
  int n2 = idx2.size();

  long long prod = static_cast<long long>(n1) * n2;

  const long long MAX_INT = 2147483647;  // 2^31 - 1

  if (prod > MAX_INT) {
    Rcpp::stop("Output matrix size exceeds 2^31-1!");
  }

  NumericMatrix subdmat(n1,n2);
  int k = 0;
  double* subdmatptr = &subdmat(0, 0);
  double* distptr = REAL(dist);
  const int* idx1ptr = idx1.begin();
  const int* idx2ptr = idx2.begin();

  for(int i=0; i<n2;i++){
    for(int j=0;j<n1;j++){
      if(idx2ptr[i] == idx1ptr[j]){
        subdmatptr[k++] = 0.0;
      } else{
        subdmatptr[k++] = distptr[indexing(N,idx2ptr[i],idx1ptr[j])];
      }
    }
  }

  return subdmat;

}

// subsetColsCpp(): Extracting a distance "matrix" object corresponding to DistanceMatrix[,colIdx]
// [[Rcpp::export(.subsetColsCpp)]]
NumericMatrix subsetColsCpp(const NumericVector &dist, const IntegerVector &colIdx){

  int N = dist.attr("Size");
  int nc = colIdx.size();
  long long prod = static_cast<long long>(N) * nc;

  const long long MAX_INT = 2147483647;  // 2^31 - 1

  if (prod > MAX_INT) {
    Rcpp::stop("Output matrix size exceeds 2^31-1!");
  }

  NumericMatrix subdmat(N,nc);
  double* subdmatptr = &subdmat(0, 0);
  double* distptr = REAL(dist);
  const int* colIdxptr = colIdx.begin();

  int m;
  int l = 0;

  for(int j = 0; j < nc; j++){
    m = indexing(N, colIdxptr[j]+1, colIdxptr[j]);

    for(int i = 0; i < colIdxptr[j]; i++){
      subdmatptr[l++] = distptr[indexing(N,colIdxptr[j],i)];
    }

    subdmatptr[l++] = 0;

    for(int k = (colIdxptr[j]+1); k < N; k++){
      subdmatptr[l++] = distptr[m];
      m++;
    }
  }
  return subdmat;
}


// Dist2MatCpp(): Converting a "dist" object to a distance "matrix" object
// [[Rcpp::export(.Dist2MatCpp)]]
NumericMatrix Dist2MatCpp(const NumericVector &dist){

  int N = dist.attr("Size");
  long long prod = static_cast<long long>(N) * N;
  const long long MAX_INT = 2147483647;  // 2^31 - 1

  if (prod > MAX_INT) {
    Rcpp::stop("Output matrix size exceeds 2^31-1!");
  }

  NumericMatrix dmat(N,N);
  int idx = 0;
  double d;
  int upperIdx;
  int lowerIdx;

  const double* distptr = REAL(dist);
  double* matptr = &dmat(0, 0);

  int iN;

  for (int i = 0; i < N-1; i++) {
    iN = i * (N+1);
    upperIdx = iN;
    lowerIdx = iN;

    for (int j = i + 1; j < N; j++, idx++) {
      lowerIdx += N;
      upperIdx ++;
      d = distptr[idx];
      matptr[upperIdx] = d;
      matptr[lowerIdx] = d;
    }

  }

  return dmat;
}



