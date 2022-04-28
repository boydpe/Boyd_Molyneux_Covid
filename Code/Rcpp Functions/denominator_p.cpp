#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix denominator_p(NumericMatrix p0, NumericMatrix time_pairs,
                       NumericVector g, NumericVector cases, 
                       NumericVector time_breaks,
                       double br, NumericVector k){
  
  int n_i = pow(p0.size(), 0.5);
  double den_p ;
  NumericMatrix p(n_i, n_i);
  double y;
 
  for (int i = 0; i < n_i; i++){
    for (int j = 0; j < n_i; j++){
      if  ( j <= i){
        
        for (int  l = 0; l < i; l++){
          y += g[time_pairs(i,l)]*k[l];//*rt[l]*h[dist_bins(i,l)]*k[mark_mat[l]];
        }
        den_p = br +y;
        if (den_p > 1e-10) {
          p(i,j) = den_p;
        } else {
          p(i,j) = den_p;//1e-10;
        }        
        y = 0;
        
      } else {
        p(i,j) = 0;
      }
    }
  }
  p(0,0) = 1;
  return p;
}

