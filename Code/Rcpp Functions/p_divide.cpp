#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix p_divide(NumericMatrix p_num, NumericMatrix p_den) {

  int n = pow(p_num.size(), 0.5); 
  NumericMatrix p_full(n,n);
  double p_val;
  
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < (i+1); j++) {
      p_val = p_num(i,j) / p_den(i,j);
      if (p_val > 1e-10) {
        p_full(i,j) = p_val;
      } else {
        p_full(i,j) = p_val;//1e-10;
      }
      
    }
  }
  return p_full;
}

