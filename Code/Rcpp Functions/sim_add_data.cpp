#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix add_data(NumericMatrix data_old, NumericVector time_bins,
                       NumericVector n_child,
                       double parent_time, int parent_gen, 
                       int parent_num, int max_num, int n_events) {
  
  NumericVector time_child;
  NumericMatrix data_new(n_events + sum(n_child), 6);
  int m = time_bins.size() - 1;
  
  // fill new matrix with old data and leave space for new events
  for(int i = 0; i < n_events; i++){
    for(int j = 0; j < 6; j++){
      data_new(i,j) = data_old(i,j);
    }
  }
  
  // fill in extra space with children events
  for(int j = 0; j < m; j++) {
    if(n_child[j] != 0) {
      for(int i = 0; i < n_child[j]; i++){
        time_child = Rcpp::runif(1, time_bins[j], time_bins[j + 1]);
        data_new(n_events + i,0) = parent_time + time_child[0];
        data_new(n_events + i,1) = parent_gen + 1;
        data_new(n_events + i,2) = ceil(data_new(n_events + i,0));
        data_new(n_events + i,3) = max_num + i;
        data_new(n_events + i,4) = parent_num;
        data_new(n_events + i,5) = 0;
      }
      max_num += n_child[j];
    }
  }
  
  

  
  return data_new;
}
