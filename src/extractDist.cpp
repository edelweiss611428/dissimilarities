#include <Rcpp.h>
using namespace Rcpp;

// indexing(): Convert 2d index to 1d index

int indexing(int nr, int i, int j){

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

// subDistCpp(): Extract a sub-distance matrix from a "dist" object
// [[Rcpp::export(.subDistCpp)]]
NumericVector subDistCpp(NumericVector dist, IntegerVector idx, bool diag, bool upper, int N, int n){

  NumericVector subDMat((n-1)*n >> 1);
  int l = 0;

  for(int i = 0; i < n-1; i++){
    for(int j = (i+1); j < n; j++){
      if(idx[i] == idx[j]){
        subDMat[l] = 0;
      } else{
        subDMat[l] = dist[indexing(N, idx[i], idx[j])];
      }
      l++;
    }
  }

  subDMat.attr("Size") = n;
  subDMat.attr("Diag") = diag;
  subDMat.attr("Upper") = upper;
  subDMat.attr("class") = "dist";

  return subDMat;
}

// extractDistCpp(): Subset a distance matrix
// [[Rcpp::export(.extractDistCpp)]]
NumericVector extractDistCpp(NumericVector dist, IntegerVector idx1, IntegerVector idx2, int N, int n1, int n2){
  NumericVector extractedDist(n1*n2);
  extractedDist.attr("dim") = Dimension(n1, n2);

  int l = 0;
  for(int i=0; i<n2;i++){
    for(int j=0;j<n1;j++){
      if(idx2[i] == idx1[j]){
        extractedDist[l] = 0;
      } else{
        extractedDist[l] = dist[indexing(N,idx2[i],idx1[j])];
      }
      l++;
    }
  }
  return extractedDist;
}




// getColumnsCpp(): Extract columns (or rows) from a symmetric distance matrix
// [[Rcpp::export(.getColumnsCpp)]]
NumericMatrix getColumns(NumericVector dist, IntegerVector ColIdx, int N, int nCol){
  NumericMatrix extractedDist(N,nCol);

  int lTemp;
  int l = 0;

  for(int j = 0; j < nCol; j++){
    lTemp = indexing(N, ColIdx[j]+1, ColIdx[j]);

    for(int i = 0; i < ColIdx[j]; i++){
      extractedDist[l] = dist[indexing(N,ColIdx[j],i)];
      l++;
    }

    extractedDist[l] = 0;
    l++;

    for(int k = (ColIdx[j]+1); k < N; k++){
      extractedDist[l] = dist[lTemp];
      l++;
      lTemp++;
    }
  }
  return extractedDist;
}
