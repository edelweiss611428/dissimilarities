#include <Rcpp.h>
using namespace Rcpp;

// indexing(): Convert a 2d index to an 1d index
inline int indexing(int nr, int i, int j){

  if(i == j){
    return NumericVector::get_na();
  }

  if (i < j+1){
    int temp;
    temp = i;
    i = j;
    j = temp;
  }

  return ((2*nr-1-j)*j >> 1) - 1 +(i-j);

}

// subsetDist2DistCpp(): Extracting a "dist" object corresponding to DistanceMatrix[idx,idx].
// [[Rcpp::export]]
NumericVector subsetDist2DistCpp(const NumericVector &dist, const IntegerVector & idx,
                                 bool diag = false, bool upper = false){
  int N = dist.attr("Size");
  int n = idx.size();
  NumericVector subdmat((n-1)*n >> 1);
  double* subdmatptr = REAL(subdmat);
  int k = 0;

  for(int i = 0; i < n-1; i++){
    for(int j = (i+1); j < n; j++){
      subdmatptr[k++] = dist[indexing(N, idx[i], idx[j])];
      k++;
    }
  }

  subdmat.attr("Size") = n;
  subdmat.attr("Diag") = diag;
  subdmat.attr("Upper") = upper;
  subdmat.attr("class") = "dist";

  return subdmat;

}


// subsetDist2MatCpp(): Extracting a distance "matrix" object corresponding to DistanceMatrix[idx1,idx2]
// [[Rcpp::export]]
NumericMatrix subsetDist2MatCpp(const NumericVector &dist, const IntegerVector &idx1, const IntegerVector &idx2){

  int N = dist.attr("Size");
  int n1 = idx1.size();
  int n2 = idx2.size();
  NumericMatrix subdmat(n1,n2);
  int k = 0;

  for(int i=0; i<n2;i++){
    for(int j=0;j<n1;j++){
      if(idx2[i] == idx1[j]){
        subdmat[k] = 0;
      } else{
        subdmat[k] = dist[indexing(N,idx2[i],idx1[j])];
      }
      k++;
    }
  }

  return subdmat;

}


// subsetColsDist2MatCpp(): Extracting a distance "matrix" object corresponding to DistanceMatrix[,colIdx]
// [[Rcpp::export]]
NumericMatrix subsetColsDist2MatCpp(const NumericVector &dist, const IntegerVector &colIdx){

  int N = dist.attr("Size");
  int nc = colIdx.size();
  NumericMatrix subdmat(N,nc);
  double* subdmatptr = &subdmat(0, 0);


  int m;
  int l = 0;

  for(int j = 0; j < nc; j++){
    m = indexing(N, colIdx[j]+1, colIdx[j]);

    for(int i = 0; i < colIdx[j]; i++){
      subdmat[l++] = dist[indexing(N,colIdx[j],i)];
    }

    subdmat[l] = 0;
    l++;

    for(int k = (colIdx[j]+1); k < N; k++){
      subdmat[l++] = dist[m];
      m++;
    }
  }
  return subdmat;
}


// Dist2MatCpp(): Converting a "dist" object to a distance "matrix" object
// [[Rcpp::export]]
NumericMatrix Dist2MatCpp(const NumericVector &dist){

  int N = dist.attr("Size");
  NumericMatrix dmat(N,N);
  int idx = 0;
  double d;
  int upperIdx;
  int lowerIdx;

  const double* distptr = REAL(dist);
  double* matptr = &dmat(0, 0);

  int iN;

  for (int i = 0; i < N; i++) {
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



