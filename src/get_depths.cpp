#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector get_depths(double px,
                         double py,
                         NumericVector &x,
                         NumericVector &y,
                         int num) {
  NumericVector depths(num);
  for (int i = 0; i < num; i++) {
    const double distance = sqrt((px-x[i])*(px-x[i]) + (py-y[i])*(py-y[i]));
    depths[i] = distance;
  }
  return depths;
}


