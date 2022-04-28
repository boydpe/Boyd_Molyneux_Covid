#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix update_df(double parent_time, int parent_gen, int n_child, 
                       int parent_num, int max_num, 
                       NumericVector whichbin, NumericVector time_bins, 
                       NumericMatrix data_old, int n_events){
  
  // NumericMatrix data_child(n_child, 6);
  NumericVector time_child;
  NumericMatrix data_new(n_events + n_child, 6);
  
  for(int i = 0; i < n_events; i++){
    for(int j = 0; j < 6; j++){
      data_new(i,j) = data_old(i,j);
    }
  }
  

  for(int i = 0; i < n_child; i++){
    time_child = Rcpp::runif(1, time_bins[whichbin[i]], time_bins[whichbin[i] + 1]);
    data_new(n_events + i,0) = parent_time + time_child[0];
    data_new(n_events + i,1) = parent_gen + 1;
    data_new(n_events + i,2) = ceil(data_new(n_events + i,0));
    data_new(n_events + i,3) = max_num + i;
    data_new(n_events + i,4) = parent_num;
    data_new(n_events + i,5) = 0;
  }
  
  return data_new;
}

