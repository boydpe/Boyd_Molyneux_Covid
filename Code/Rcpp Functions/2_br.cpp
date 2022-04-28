#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double calc_br( NumericMatrix p0, NumericVector newcases, int ndays) {
  
  double br;
  double diag_sum = 0;
  int n = pow(p0.size(), 0.5);

  for (int i = 0; i < n; i++) {
      diag_sum += p0(i,i)*newcases[i];
  }
  br = diag_sum/ndays;
  return br;
}
