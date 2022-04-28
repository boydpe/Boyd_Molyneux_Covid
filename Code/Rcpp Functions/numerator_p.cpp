#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix numerator_p(NumericMatrix p0, NumericMatrix time_pairs,
                       NumericVector g, NumericVector cases, 
                       NumericVector time_breaks,
                       double br, NumericVector k){
  
  int n_i = pow(p0.size(), 0.5);
  double num_p;
  double den_p ;
  NumericMatrix p(n_i, n_i);

  for (int i = 0; i < n_i; i++){
    for (int j = 0; j < n_i; j++){
      if  ( j < i){
        num_p = g[time_pairs(i,j)]*k[j];//*rt[j]
        if (num_p > 1e-10) {
          p(i,j) = num_p;
        } else {
          p(i,j) = num_p;//1e-10;
        }
        
      } else if (i == j) {
        num_p = br;
        if (num_p > 1e-10) {
          p(i,j) = num_p;
        } else {
          p(i,j) = num_p;//1e-10;
        } 
      }
      
      else {
        p(i,j) = 0;
      }
    }
  }
  p(0,0) = 1;
  return p;
}

