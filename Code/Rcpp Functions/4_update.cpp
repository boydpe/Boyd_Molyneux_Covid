#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix update_p(NumericMatrix p0,
                       NumericVector g, NumericVector cases, 
                       NumericVector time_breaks,
                        NumericVector br, NumericVector rt){

  int n_i = pow(p0.size(), 0.5);
  double num_p;
  double den_p ;
  NumericMatrix p(n_i, n_i);
  double y;
  // const double pi = 3.14159265358979323846;
  // double pi = M_PI;
  // double sum_mat =0;
  // NumericVector Rt(n_i);

  // for (int i = 0; i < n_i; i++) {
  //   for ( int j = 0; j < n_i; j++){
  //     sum_mat += dist_mat(i,j);
  //   }
  // }

  // if ( sum(lat) != 0){
  //   for (int i = 0; i < n_i; i++){
  //     for (int j = 0; j < n_i; j++){
  //       if  ( j < i){
  //         num_p = (g[time_bins(j,i)]*h[dist_bins(i,j)] *
  //           k[mark_mat[j]]); // /(2*pi*dist_mat(i,j)));
  // 
  //         for (int  l = 0; l < i; l++){
  //           y += g[time_bins(l,i)]*h[dist_bins(i,l)]*k[mark_mat[l]]; // /
  //             //(2*pi*dist_mat(i,l));
  //         }
  //         den_p = br[i] + y;
  //         p(i,j) = num_p / den_p;
  //         y = 0;
  // 
  //       } else if (i == j) {
  //         num_p = br[i];
  // 
  //         for (int  l = 0; l < i; l++){
  //           y += g[time_bins(l,i)]*h[dist_bins(i,l)]*k[mark_mat[l]]; ///
  //             //(2*pi*dist_mat(i,l));
  //         }
  //         den_p = br[i] + y;
  //         p(i,j) = num_p / den_p;
  //         y = 0;
  //       } else {
  //         p(i,j) = 0;
  //       }
  //     }
  //   }
  //   p(0,0) = 1;
  //   return p;
  // } else {
  // Rt = (colSums(p0) - diag(p0)) * cases;
    for (int i = 0; i < n_i; i++){
      for (int j = 0; j < n_i; j++){
        if  ( j < i){
          num_p = g[i-j]*rt[j];
          // *h[dist_bins(i,j)] *k[mark_mat[j]];

          for (int  l = 0; l < i; l++){
            y += g[i-l]*rt[l];//*h[dist_bins(i,l)]*k[mark_mat[l]];
          }
          den_p = br[i] +y;
          p(i,j) = num_p / den_p;
          y = 0;

        }else if (i == j) {
          num_p = br[i];

          for (int  l = 0; l < i; l++){
            y += g[i-l]*rt[l];// *h[dist_bins(i,l)]*k[mark_mat[l]];
          }
          den_p = br[i] + y;
          p(i,j) = num_p / den_p;
          y = 0;
        }

        else {
          p(i,j) = 0;
        }
      }
    }
    p(0,0) = 1;
    return p;
  }

