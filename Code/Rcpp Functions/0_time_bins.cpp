#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix get_time_bins( NumericVector times, NumericVector time_breaks) {

  int n = times.size();
  int n_bins = time_breaks.size() - 1;
  NumericMatrix time_bins(n,n);

  for (int j = 0;j < n; j++) {
    for (int i = 0; i < n; i++) {
      for (int k = 0; k < n_bins; k++) {
        if ((times[i] - times[j]) >= time_breaks[k] && (times[i] - times[j]) < time_breaks[k + 1]) {
          time_bins(i,j) = k;
        }
      }
    }
  }
  return time_bins;
}
