#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector get_rep_number(NumericVector rt, NumericVector time_bins, NumericVector times) {
  
  int n = rt.size();
  NumericVector Rt(n);
  
  for(int i = 0; i < n; i++) {
    for(int j = 0; j < n; j++) {
      if(times[j] >= time_bins[i] && times[j] < time_bins[i +1]) {
        Rt[i] += rt[i];
      }
    }
  }
  return Rt;
}


