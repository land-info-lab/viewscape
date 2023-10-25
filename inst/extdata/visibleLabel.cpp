#include "iostream"
#include <fstream>
#include <string>
#include <Rcpp.h>
#include <algorithm>
#include <chrono>

using namespace Rcpp;

//[[Rcpp::export]]
Rcpp::IntegerMatrix visibleLabel(
    const NumericVector &viewpoint,
    const NumericMatrix &dsm,
    const double h) {

  NumericVector zl;
  NumericVector xl;
  NumericVector yl;
  const int rows = dsm.rows();
  const int cols = dsm.cols();
  int steps;
  IntegerMatrix visible(rows, cols);

  auto start = std::chrono::system_clock::now();
  for (int i = 0; i < rows; i++) {
     for (int j = 0; j < cols; j++) {
       const double z = dsm(i,j) + h;
       steps = sqrt((viewpoint[0]-j)*(viewpoint[0]-j) + (viewpoint[1]-i)*(viewpoint[1]-i));
       NumericVector sequence(steps);
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
     }
  }
  auto end = std::chrono::system_clock::now();
  std::chrono::duration<double> elapsed_seconds = end-start;
  std::cout << "elapsed time: " << elapsed_seconds.count() << "s"
            << std::endl;
  return visible;
}
