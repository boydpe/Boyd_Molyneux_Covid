#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix get_g_ee(NumericMatrix p0, NumericVector time_breaks, 
                       NumericVector times, NumericVector cases) {
  
  // numerator
  int n_bins = time_breaks.size() - 1;
  int n_tot = pow(p0.size(), 0.5);
  NumericVector num_g(n_bins);
  NumericVector den_g(n_bins);
  NumericVector den_t(n_bins);
  // double diag_sum = 0;
  NumericMatrix g_vals(n_bins, 2);
  double case_trig = 0;
  
  
  for (int l = 0; l < n_bins; l++) {
    for (int i = 0; i < n_tot; i++){
      for (int j = i; j < n_tot; j++){
        if(time_breaks[l] <= (times[j] - times[i]) && 
           (times[j]-times[i]) < time_breaks[l + 1]){
          num_g[l] += p0(j,i) * cases[j];
        } else{
          num_g[l] += 0;
        }
      }
    }
  }
  
  // denominator
  
  for (int i = 0; i < n_bins; i++){
    den_t[i] = (time_breaks[i + 1] - time_breaks[i]);
  }
  
  // for (int i = 0; i < n_i; i++) {
  //   diag_sum += p0(i,i);
  // }
  
  for (int i = 1; i < n_tot; i++) {
    for (int j = 0; j < i; j++) {
      case_trig += p0(i,j)*cases[i];
    }
  }
  
  // den_g = den_t * (sum(p0) - sum(diag(p0)));
  den_g = den_t * case_trig;
  
  for (int i = 0; i < n_bins; i++) {
    g_vals(i,0) = num_g[i];
    g_vals(i,1) = den_g[i];
  }
  return g_vals;
}

