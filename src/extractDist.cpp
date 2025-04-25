#include <Rcpp.h>
using namespace Rcpp;

// indexing(): Convert a 2d index to an 1d index
inline int indexing(int nr, int i, int j){

  if(i == j){
    return 0;
  }

  if (i < j+1){
    int temp;
    temp = i;
    i = j;
    j = temp;
  }

  return ((2*nr-1-j)*j >> 1) - 1 +(i-j);

}

// subsetDist2DistCpp(): Extracting a "dist" object corresponding to DataMatrix[idx,idx].
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


// subsetDist2MatCpp(): Extracting a "Matrix" object corresponding to DataMatrix[idx1,idx2]
// [[Rcpp::export]]
NumericVector subsetDist2MatCpp(NumericVector dist, IntegerVector idx1, IntegerVector idx2,
                                bool diag = false, bool upper = false){

  int N = dist.attr("Size");
  int n1 = idx1.size();
  int n2 = idx2.size();
  NumericVector subdmat(n1*n2);
  subdmat.attr("dim") = Dimension(n1, n2);
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
