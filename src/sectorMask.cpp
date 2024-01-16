#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
double sectorValue(
    const double k,
    const double b,
    const int x) {
  double result;
  result = k*x + b;
  return result;
}

Rcpp::IntegerMatrix sectorMask(
    const Rcpp::IntegerMatrix &viewshed,
    const Rcpp::NumericVector viewpt,
    const Rcpp::NumericVector fov) {
  const int rows = viewshed.rows();
  const int cols = viewshed.cols();
  Rcpp::IntegerMatrix visible(rows, cols);
  const double factor = 3.14159/180;
  const double ka = tan(fov[0]*factor);
  const double kb = tan(fov[1]*factor);
  const double ba = -viewpt[1] - ka*viewpt[0];
  const double bb = -viewpt[1] - kb*viewpt[0];

  for (int i = 0; i < cols; i++) {
    for (int j = 0; j < rows; j++) {
      int y = -j;
      double a = sectorValue(ka, ba, i);
      double b = sectorValue(kb, bb, i);
      if (fov[0] < fov[1]) {
        if (fov[1]-fov[0] < 180){
          if (fov[0] <= 90) {
            if (a <= y && b <= y) {
              visible(j,i) = viewshed(j,i);
            } else {
              visible(j,i) = 0;
            }
          } else if (fov[0] > 90 && fov[0] <= 270) {
            if (fov[1] <= 270) {
              if (a >= y && b <= y) {
                visible(j,i) = viewshed(j,i);
              } else {
                visible(j,i) = 0;
              }
            } else {
              if (a >= y && b >= y) {
                visible(j,i) = viewshed(j,i);
              } else {
                visible(j,i) = 0;
              }
            }
          } else if (fov[0] > 270 && fov[0] < 360) {
            if (a <= y && b >= y) {
              visible(j,i) = viewshed(j,i);
            } else {
              visible(j,i) = 0;
            }
          }
        } else if (fov[1]-fov[0] > 180) {

        }
      }
    }
  }
  return visible;
}
