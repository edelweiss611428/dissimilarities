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
NumericVector subsetDist2DistCpp(NumericVector dist, IntegerVector idx,
                                 bool diag = false, bool upper = false){
  int N = dist.attr("Size");
  int n = idx.size();
  NumericVector subdmat((n-1)*n >> 1);
  int k = 0;

  for(int i = 0; i < n-1; i++){
    for(int j = (i+1); j < n; j++){
      subdmat[k++] = dist[indexing(N, idx[i], idx[j])];
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
NumericMatrix subsetDist2MatCpp(NumericVector dist, IntegerVector idx1, IntegerVector idx2){

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


// getColsCpp(): Extracting a distance "matrix" object corresponding to DistanceMatrix[,idx]
// [[Rcpp::export]]
NumericMatrix getColsCpp(NumericVector dist, IntegerVector colIdx){

  int N = dist.attr("Size");
  int nc = colIdx.size();
  NumericMatrix subdmat(N,nc);

  int m;
  int l = 0;

  for(int j = 0; j < nc; j++){
    m = indexing(N, colIdx[j]+1, colIdx[j]);

    for(int i = 0; i < colIdx[j]; i++){
      subdmat[l] = dist[indexing(N,colIdx[j],i)];
      l++;
    }

    subdmat[l] = 0;
    l++;

    for(int k = (colIdx[j]+1); k < N; k++){
      subdmat[l] = dist[m];
      l++;
      m++;
    }
  }
  return subdmat;
}


// Dist2MatCpp(): Converting a "dist" object to a distance "matrix" object
// [[Rcpp::export]]
NumericMatrix Dist2MatCpp(NumericVector dist){
  int N = dist.attr("Size");
  NumericMatrix dmat(N,N);
  int idx = 0;

  for (int i = 0; i < N; i++) {
    for (int j = i + 1; j < N; j++) {
      dmat(i, j) = dist[idx];
      dmat(j, i) = dist[idx];
      idx++;
    }
  }

  return dmat;
}



