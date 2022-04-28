#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double calc_br_ee( NumericMatrix p0, int days, int ee_date ) {
  
  double br;
  double diag_sum = 0;
  // int n = pow(p0.size(), 0.5);
  
  for (int i = 0; i < ee_date; i++) {
    diag_sum += p0(i,i);
  }
  br = diag_sum/ee_date;
  return br;
}
