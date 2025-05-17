#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;
// get1dFrom2dCpp(): 2d to 1d index conversion
// [[Rcpp::export(.get1dFrom2dCpp)]]
int get1dFrom2dCpp(int nr, int i, int j){

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