#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double pick_k(double current_time, NumericVector k, NumericVector duration) {
  
  double k_val;
  if(current_time <= duration[0]) {
    k_val = k[0];
  } else if(current_time <= (duration[0] + duration[1]) &
    current_time > duration[0]) {
    k_val = k[1];
  } else {
    k_val = k[2];
  }
  
  return k_val;
}

