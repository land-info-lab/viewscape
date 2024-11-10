#include <Rcpp.h>
#include<cmath>
using namespace Rcpp;

struct Vector3 {
  double x, y, z;
};

double calculateViewSlope(double distance, Vector3 view, double z) {
  double dz = z - view.z;
  double slope = atan2(distance, dz);
  return slope;
}

double calculateViewAspect(int row, int col, Vector3 view) {
  double pi = 2*asin(1.0);
  double aspect = int(-atan2(view.x - col, view.y - row) + 2 * pi) % int(2 * pi);
  return aspect;
}

Vector3 calculateNormal(double aspect, double slope,
                        int col, int row, double z) {
  Vector3 normal;
  normal = {cos(aspect) * sin(slope),
            sin(aspect) * sin(slope),
            cos(slope)};
  // normalize
  double length = sqrt(pow(normal.x, 2) +
                       pow(normal.y, 2) +
                       pow(normal.z, 2));
  normal.x /= length;
  normal.y /= length;
  normal.z /= length;
  normal = {double(col) - normal.x, double(row) - normal.y, z - normal.z};
  return normal;
}

double getAngle(Vector3 a, Vector3 b) {
  double radian;
  double dot = a.x*b.x + a.y*b.y + a.z*b.z;
  double mold1 = sqrt(pow(a.x, 2) + pow(a.y, 2) + pow(a.z, 2));
  double mold2 = sqrt(pow(b.x, 2) + pow(b.y, 2) + pow(b.z, 2));
  radian = acos(dot/(mold1*mold2));
  return radian;
}

// double cosAB(int xyp, double zp,
//              int xyt, double zt,
//              double xyn, double zn) {
//   double res;
//   const double pn = sqrt(pow(xyp-xyn, 2) + pow(zp-zn, 2));
//   const double pt = sqrt(pow(xyp-xyt, 2) + pow(zp-zt, 2));
//   const double tn = sqrt(pow(xyn-xyt, 2) + pow(zn-zt, 2));
//   res = (pow(pt,2) + pow(tn,2) - pow(pn,2))/(2*pt*tn);
//   return res;
// }

double PTdistance(int xp, int yp, double zp,
                  int xt, int yt, double zt,
                  int resolution) {
  double res = sqrt(pow((xp-xt)*resolution,2)+
                    pow((yp-yt)*resolution,2)+
                    pow((zp-zt),2));
  return res;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix VM(const Rcpp::IntegerMatrix &viewshed,
                       const Rcpp::IntegerMatrix &dsm,
                       const Rcpp::NumericMatrix &slp,
                       const Rcpp::NumericMatrix &asp,
                       const Rcpp::NumericVector viewpt,
                       const double h,
                       const int resolution) {
  const int rows = dsm.rows();
  const int cols = dsm.cols();
  const double zp = dsm(viewpt[1],viewpt[0]) + h;
  Rcpp::NumericMatrix magnitude(rows, cols);
  double viewSlope, viewAspect;
  Vector3 view = {double(viewpt[0]), double(viewpt[1]), zp};

  for (int i = 0; i < cols; i++) {
    for (int j = 0; j < rows; j++) {
      if (i != viewpt[0] && j != viewpt[1]) {
        if (viewshed(j,i) > 0) {
          double zt = dsm(j,i);
          double slope = slp(j,i);
          int aspect = asp(j,i);
          double dis = PTdistance(viewpt[0], viewpt[1], zp,
                                  i, j, zt, resolution);
          viewSlope = calculateViewSlope(dis, view, zt);
          viewAspect = calculateViewAspect(j, i, view);
          Vector3 viewNormal = calculateNormal(viewAspect, viewSlope,
                                               viewpt[0], viewpt[1], zp);
          Vector3 normal = calculateNormal(aspect, slope, i, j, zt);
          double radian = getAngle(viewNormal, normal);
          if (radian < 3.1415926/2) {
            magnitude(j, i) = std::fabs(cos(radian)) * resolution*resolution/(dis*dis);
          }
        } else {
          magnitude(j, i) = -9;
        }
      }
    }
  }
  return magnitude;
}
