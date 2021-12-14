#include <Rcpp.h>
#include <cmath>
#include <vector>
using namespace Rcpp;

// this function is used to categorize a single variable

// [[Rcpp::export]]
NumericVector categorization(NumericVector x){
  int n = x.length();
  NumericVector x_new(n);
  
  for(int i=0; i < n; i++){
    double a = x(i);
    if(a <= 6){
      x_new(i) = 1;
    }else if(a <= 24){
      x_new(i) = 2;
    }else{
      x_new(i) = 3;
    }
  }
  return x_new;
}