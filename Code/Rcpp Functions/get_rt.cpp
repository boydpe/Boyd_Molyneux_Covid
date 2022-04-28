#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector get_rt(NumericMatrix p0,
                       NumericVector cases){
  
  int n = cases.size();
  NumericVector rt(n);
  rt = (colSums(p0) - diag(p0)) / cases;
  return rt;
}

