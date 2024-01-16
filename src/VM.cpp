#include <Rcpp.h>
#include<cmath>
using namespace Rcpp;

// [[Rcpp::export]]
double cosAB(int xyp, double zp,
             int xyt, double zt,
             int xyn, double zn) {
  double res;
  const double pn = sqrt(pow(xyp-xyn, 2) + pow(zp-zn, 2));
  const double pt = sqrt(pow(xyp-xyt, 2) + pow(zp-zt, 2));
  const double tn = sqrt(pow(xyn-xyt, 2) + pow(zn-zt, 2));
  res = (pow(pt,2) + pow(tn,2) - pow(pn,2))/(2*pt*tn);
  return res;
}

Rcpp::NumericVector normalVector(int slope, int direction,
                                 int x, int y, double z) {
  Rcpp::NumericVector res(3);
  double zn;
  int xn;
  int yn;
  if (direction == 1 || direction == 4 || direction == 16 || direction == 64) {
    zn = tan(90-slope)+z;
    if (direction == 4 || direction == 64) {
      xn = x;
      if (direction == 4) {
        yn = y + 1;
      } else {
        yn = y - 1;
      }
    } else if (direction == 1 || direction == 16) {
      yn = y;
      if (direction == 1) {
        xn = x + 1;
      } else {
        xn = x - 1;
      }
    }
  } else if (direction == 2 || direction == 8 || direction == 32 || direction == 128) {
    zn = tan(90-slope)*sqrt(2)+z;
    if (direction == 2) {
      xn = x + 1;
      yn = y + 1;
    } else if (direction == 8) {
      xn = x - 1;
      yn = y + 1;
    } else if (direction == 32) {
      xn = x - 1;
      yn = y - 1;
    } else if (direction == 128) {
      xn = x + 1;
      yn = y - 1;
    }
  }
  res[0] = xn;
  res[1] = yn;
  res[2] = zn;
  return res;
}

double PTdistance(int xp, int yp, double zp,
                  int xt, int yt, double zt) {
  double res = sqrt(pow(xp-xt,2)+pow(yp-yt,2)+pow(zp-zt,2.0));
  return res;
}

Rcpp::IntegerMatrix VM(const Rcpp::IntegerMatrix &viewshed,
                       const Rcpp::IntegerMatrix &slp,
                       const Rcpp::IntegerMatrix &dir,
                       const Rcpp::NumericVector viewpt) {
  const int rows = viewshed.rows();
  const int cols = viewshed.cols();
  Rcpp::IntegerMatrix magnitude(rows, cols);
  for (int i = 0; i < cols; i++) {
    for (int j = 0; j < rows; j++) {
      double zt = viewshed(j,i);
      double zp = viewshed(viewpt[1],viewpt[0]);
      int slope = slp(j,i);
      int direction = dir(j,i);
      double dis = PTdistance(viewpt[0], viewpt[1], zp,
                              i, j, zt);
      Rcpp::NumericVector normal = normalVector(slope, direction,
                                                i, j, zt);
      double cosA = cosAB(viewpt[0], zp, i, zt,
                          normal[0], normal[2]);
      double cosB = cosAB(viewpt[1], zp, j, zt,
                          normal[1], normal[2]);
      magnitude(j, i) = cosA * cosB / dis;
    }
  }
  return magnitude;
}
