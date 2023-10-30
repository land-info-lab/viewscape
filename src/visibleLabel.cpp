#include "iostream"
#include <string>
#include <Rcpp.h>
#include <fstream>
#include <algorithm>

using namespace Rcpp;

//[[Rcpp::export]]
Rcpp::IntegerMatrix visibleLabel(
    const Rcpp::NumericVector &viewpoint,
    const Rcpp::NumericMatrix &dsm,
    const double h,
    const int max_dis) {

  Rcpp::NumericVector zl;
  Rcpp::NumericVector xl;
  Rcpp::NumericVector yl;
  const int rows = dsm.rows();
  const int cols = dsm.cols();
  int steps;
  Rcpp::IntegerMatrix visible(rows, cols);

  for (int i = 0; i < rows; i++) {
     for (int j = 0; j < cols; j++) {
       steps = sqrt((viewpoint[0]-j)*(viewpoint[0]-j) + (viewpoint[1]-i)*(viewpoint[1]-i));
       const double z = dsm(i,j) + h;
       if (steps <= max_dis) {
         Rcpp::NumericVector sequence(steps);
         std::iota(sequence.begin(), sequence.end(), 1);
         xl = viewpoint[0] + sequence * (j-viewpoint[0])/steps;
         yl = viewpoint[1] + sequence * (i-viewpoint[1])/steps;

         if(viewpoint[2]<z) {
           zl = viewpoint[2] + sequence/steps*fabs(viewpoint[2]-z);
         } else if (viewpoint[2]>z) {
           zl = viewpoint[2] - sequence/steps*fabs(viewpoint[2]-z);
         } else if (viewpoint[2]==z) {
           zl = viewpoint[2] + 0*sequence;
         }
         int temp = 1;
         for (int p = 0; p < steps; p++) {
           const double d = zl[p] - dsm(yl[p],xl[p]);
           if (d < 0) {
             temp = 0;
             break;
           }
         }
         visible(i,j) = temp;
       } else {
         visible(i,j) = 0;
       }
     }
  }
  return visible;
}
