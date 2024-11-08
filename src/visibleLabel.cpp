#include <iostream>
#include <vector>
#include <string>
#include <Rcpp.h>
#include <fstream>
#include <algorithm>

using namespace Rcpp;

struct Vector3 {
  double x, y, z;
};

Vector3 crossProduct(const Vector3& a, const Vector3& b) {
  Vector3 result;
  result.x = a.y * b.z - a.z * b.y;
  result.y = a.z * b.x - a.x * b.z;
  result.z = a.x * b.y - a.y * b.x;
  return result;
}

Vector3 vectorSubtract(const Vector3& end, const Vector3& start) {
  Vector3 result;
  result.x = end.x - start.x;
  result.y = end.y - start.y;
  result.z = end.z - start.z;
  return result;
}

std::vector<double> computePlane(const Vector3& viewpoint,
                                 const Vector3& p1,
                                 const Vector3& p2) {
  Vector3 vp1 = vectorSubtract(p1, viewpoint);
  Vector3 vp2 = vectorSubtract(p2, viewpoint);
  Vector3 normal = crossProduct(vp1, vp2);
  double A = normal.x;
  double B = normal.y;
  double C = normal.z;
  double D = -(A * viewpoint.x + B * viewpoint.y + C * viewpoint.z);
  return {A, B, C, D};
}

double zOnPlane(const Vector3& p, const std::vector<double>& plane) {
  return -(plane[0]*p.x + plane[1]*p.y + plane[3])/plane[2];
}

Rcpp::IntegerMatrix wswSector(const Vector3 &viewpt,
                              const Rcpp::NumericMatrix &dsm,
                              Rcpp::IntegerMatrix visible,
                              int rows,
                              int max_dis,
                              const double h) {
  Rcpp::NumericMatrix referenceGrid = dsm;
  double minElevation;
  Vector3 target;
  Vector3 temp_1;
  Vector3 temp_2;
  std::vector<double> referencePlane;

  int count = 0;
  for (int j = viewpt.x - 2; j > 0; --j) {
    count++;
    for (int i = viewpt.y + 1; i < rows && count >= i-viewpt.y; i++) {
      if (j + 1 < dsm.ncol() && i < dsm.nrow() && i - 1 >= 0) {
        temp_1 = {double(j+1), double(i), referenceGrid(double(i), double(j+1))};
        temp_2 = {double(j+1), double(i-1), referenceGrid(double(i-1), double(j+1))};
        if (sqrt((viewpt.x-j)*(viewpt.x-j) + (viewpt.y-i)*(viewpt.y-i)) <= max_dis) {
          target = {double(j), double(i), dsm(i,j)+h};
          referencePlane = computePlane(viewpt, temp_1, temp_2);
          minElevation = zOnPlane(target, referencePlane);
          if (target.z > minElevation) {
            visible(i,j) = 1;
          } else {
            referenceGrid(i,j) = minElevation;
          }
        }
      }
    }
  }
  return visible;
}

Rcpp::IntegerMatrix wnwSector(const Vector3 &viewpt,
                              const Rcpp::NumericMatrix &dsm,
                              Rcpp::IntegerMatrix visible,
                              int max_dis,
                              const double h) {
  Rcpp::NumericMatrix referenceGrid = dsm;
  double minElevation;
  Vector3 target;
  Vector3 temp_1;
  Vector3 temp_2;
  std::vector<double> referencePlane;

  int count = 0;
  for (int j = viewpt.x - 2; j > 0; --j) {
    count++;
    for (int i = viewpt.y - 1; i > 0 && count >= viewpt.y - i; --i) {
      temp_1 = {double(j+1), double(i), referenceGrid(double(i), double(j+1))};
      temp_2 = {double(j+1), double(i+1), referenceGrid(double(i+1), double(j+1))};
      if (sqrt((viewpt.x-j)*(viewpt.x-j) + (viewpt.y-i)*(viewpt.y-i)) <= max_dis) {
        target = {double(j), double(i), dsm(i,j)+h};
        referencePlane = computePlane(viewpt, temp_1, temp_2);
        minElevation = zOnPlane(target, referencePlane);
        if (target.z > minElevation) {
          visible(i,j) = 1;
        } else {
          referenceGrid(i,j) = minElevation;
        }
      }
    }
  }
  return visible;
}

Rcpp::IntegerMatrix nwnSector(const Vector3 &viewpt,
                              const Rcpp::NumericMatrix &dsm,
                              Rcpp::IntegerMatrix visible,
                              int max_dis,
                              const double h) {
  Rcpp::NumericMatrix referenceGrid = dsm;
  double minElevation;
  Vector3 target;
  Vector3 temp_1;
  Vector3 temp_2;
  std::vector<double> referencePlane;

  int count = 0;
  for (int i = viewpt.y - 2; i > 0; --i) {
    count++;
    for (int j = viewpt.x - 1; j > 0 && count >= viewpt.x - j; --j) {
      temp_1 = {double(j), double(i+1), referenceGrid(double(i+1), double(j))};
      temp_2 = {double(j+1), double(i+1), referenceGrid(double(i+1), double(j+1))};
      if (sqrt((viewpt.x-j)*(viewpt.x-j) + (viewpt.y-i)*(viewpt.y-i)) <= max_dis) {
        target = {double(j), double(i), dsm(i,j)+h};
        referencePlane = computePlane(viewpt, temp_1, temp_2);
        minElevation = zOnPlane(target, referencePlane);
        if (target.z > minElevation) {
          visible(i,j) = 1;
        } else {
          referenceGrid(i,j) = minElevation;
        }
      }
    }
  }
  return visible;
}

Rcpp::IntegerMatrix nenSector(const Vector3 &viewpt,
                              const Rcpp::NumericMatrix &dsm,
                              Rcpp::IntegerMatrix visible,
                              int cols,
                              int max_dis,
                              const double h) {
  Rcpp::NumericMatrix referenceGrid = dsm;
  double minElevation;
  Vector3 target;
  Vector3 temp_1;
  Vector3 temp_2;
  std::vector<double> referencePlane;

  int count = 0;
  for (int i = viewpt.y - 2; i > 0; --i) {
    count++;
    for (int j = viewpt.x + 1; j < cols && count >= j-viewpt.x; j++) {
      temp_1 = {double(j), double(i+1), referenceGrid(double(i+1), double(j))};
      temp_2 = {double(j-1), double(i+1), referenceGrid(double(i+1), double(j-1))};
      if (sqrt((viewpt.x-j)*(viewpt.x-j) + (viewpt.y-i)*(viewpt.y-i)) <= max_dis) {
        target = {double(j), double(i), dsm(i,j)+h};
        referencePlane = computePlane(viewpt, temp_1, temp_2);
        minElevation = zOnPlane(target, referencePlane);
        if (target.z > minElevation) {
          visible(i,j) = 1;
        } else {
          referenceGrid(i,j) = minElevation;
        }
      }
    }
  }
  return visible;
}

Rcpp::IntegerMatrix eneSector(const Vector3 &viewpt,
                              const Rcpp::NumericMatrix &dsm,
                              Rcpp::IntegerMatrix visible,
                              int cols,
                              int max_dis,
                              const double h) {
  Rcpp::NumericMatrix referenceGrid = dsm;
  double minElevation;
  Vector3 target;
  Vector3 temp_1;
  Vector3 temp_2;
  std::vector<double> referencePlane;

  int count = 0;
  for (int j = viewpt.x + 2; j < cols; j++) {
    count++;
    for (int i = viewpt.y - 1; i > 0 && count >= viewpt.y-i; --i) {
      temp_1 = {double(j-1), double(i), referenceGrid(double(i), double(j-1))};
      temp_2 = {double(j-1), double(i+1), referenceGrid(double(i+1), double(j-1))};
      if (sqrt((viewpt.x-j)*(viewpt.x-j) + (viewpt.y-i)*(viewpt.y-i)) <= max_dis) {
        target = {double(j), double(i), dsm(i,j)+h};
        referencePlane = computePlane(viewpt, temp_1, temp_2);
        minElevation = zOnPlane(target, referencePlane);
        if (target.z > minElevation) {
          visible(i,j) = 1;
        } else {
          referenceGrid(i,j) = minElevation;
        }
      }
    }
  }
  return visible;
}

Rcpp::IntegerMatrix eseSector(const Vector3 &viewpt,
                              const Rcpp::NumericMatrix &dsm,
                              Rcpp::IntegerMatrix visible,
                              int rows,
                              int cols,
                              int max_dis,
                              const double h) {
  Rcpp::NumericMatrix referenceGrid = dsm;
  double minElevation;
  Vector3 target;
  Vector3 temp_1;
  Vector3 temp_2;
  std::vector<double> referencePlane;

  int count = 0;
  for (int j = viewpt.x + 2; j < cols; j++) {
    count++;
    for (int i = viewpt.y + 1; i < rows && count >= i-viewpt.y; i++) {
      temp_1 = {double(j-1), double(i), referenceGrid(double(i), double(j-1))};
      temp_2 = {double(j-1), double(i-1), referenceGrid(double(i-1), double(j-1))};
      if (sqrt((viewpt.x-j)*(viewpt.x-j) + (viewpt.y-i)*(viewpt.y-i)) <= max_dis) {
        target = {double(j), double(i), dsm(i,j)+h};
        referencePlane = computePlane(viewpt, temp_1, temp_2);
        minElevation = zOnPlane(target, referencePlane);
        if (target.z > minElevation) {
          visible(i,j) = 1;
        } else {
          referenceGrid(i,j) = minElevation;
        }
      }
    }
  }
  return visible;
}

Rcpp::IntegerMatrix sesSector(const Vector3 &viewpt,
                              const Rcpp::NumericMatrix &dsm,
                              Rcpp::IntegerMatrix visible,
                              int rows,
                              int cols,
                              int max_dis,
                              const double h) {
  Rcpp::NumericMatrix referenceGrid = dsm;
  double minElevation;
  Vector3 target;
  Vector3 temp_1;
  Vector3 temp_2;
  std::vector<double> referencePlane;

  int count = 0;
  for (int i = viewpt.y + 2; i < rows; i++) {
    count++;
    for (int j = viewpt.x + 1; j < cols && count >= j-viewpt.x; j++) {
      temp_1 = {double(j), double(i-1), referenceGrid(double(i-1), double(j))};
      temp_2 = {double(j-1), double(i-1), referenceGrid(double(i-1), double(j-1))};
      if (sqrt((viewpt.x-j)*(viewpt.x-j) + (viewpt.y-i)*(viewpt.y-i)) <= max_dis) {
        target = {double(j), double(i), dsm(i,j)+h};
        referencePlane = computePlane(viewpt, temp_1, temp_2);
        minElevation = zOnPlane(target, referencePlane);
        if (target.z > minElevation) {
          visible(i,j) = 1;
        } else {
          referenceGrid(i,j) = minElevation;
        }
      }
    }
  }
  return visible;
}

Rcpp::IntegerMatrix swsSector(const Vector3 &viewpt,
                              const Rcpp::NumericMatrix &dsm,
                              Rcpp::IntegerMatrix visible,
                              int max_dis,
                              int rows,
                              const double h) {
  Rcpp::NumericMatrix referenceGrid = dsm;
  double minElevation;
  Vector3 target;
  Vector3 temp_1;
  Vector3 temp_2;
  std::vector<double> referencePlane;

  int count = 0;
  for (int i = viewpt.y + 2; i < rows; i++) {
    count++;
    for (int j = viewpt.x - 1; j > 0 && count >= viewpt.x - j; --j) {
      temp_1 = {double(j), double(i-1), referenceGrid(double(i-1), double(j))};
      temp_2 = {double(j+1), double(i-1), referenceGrid(double(i-1), double(j+1))};
      if (sqrt((viewpt.x-j)*(viewpt.x-j) + (viewpt.y-i)*(viewpt.y-i)) <= max_dis) {
        target = {double(j), double(i), dsm(i,j)+h};
        referencePlane = computePlane(viewpt, temp_1, temp_2);
        minElevation = zOnPlane(target, referencePlane);
        if (target.z > minElevation) {
          visible(i,j) = 1;
        } else {
          referenceGrid(i,j) = minElevation;
        }
      }
    }
  }
  return visible;
}

//Rcpp::IntegerMatrix updatePlane(const Vector3 &viewpt, Vector3 t1, Vector3 t2)

//Rcpp::IntegerMatrix referencePlaneVisible(
// const int direction,
// const Vector3 &viewpt,
// const Rcpp::NumericMatrix &dsm,
// Rcpp::IntegerMatrix& visible,
// int rows,
// int cols,
// int max_dis,
// const double h)

int compareTan(const double disa,
               const double disb,
               const double ha,
               const double hb) {
  int result = 0;
  if (ha/disa > hb/disb) {
    result = 1;
  }
  return result;
}

double horizontalDis(const Vector3& p1, const Vector3& p2) {
  double result = sqrt(pow(p1.x-p2.x,2) + pow(p1.y-p2.y,2));
  return result;
}

Vector3 updateLine(const Vector3 &viewpt,
                   const Vector3 target,
                   Vector3 temp,
                   int max_dis) {
  double distance_a = horizontalDis(target, viewpt);
  double distance_b = horizontalDis(temp, viewpt);
  if (distance_a <= max_dis) {
    double h_a = target.z - viewpt.z;
    double h_b = temp.z - viewpt.z;
    if (h_b < 0) {
      if (h_a >= 0) {
        temp = target;
      } else {
        if (compareTan(distance_a, distance_b, -h_a, -h_b) == 0) {
          temp = target;
        }
      }
    } else {
      if (h_a > 0) {
        if (compareTan(distance_a, distance_b, h_a, h_b) == 1) {
          temp = target;
        }
      }
    }
  }
  return temp;
}

Rcpp::IntegerMatrix referenceLineVisible(
    const Vector3 &viewpt,
    const Rcpp::NumericMatrix &dsm,
    Rcpp::IntegerMatrix& visible,
    int rows,
    int cols,
    int max_dis,
    const double h) {
  Vector3 temp;
  // to N
  if (viewpt.y >= 2) {
    temp = {viewpt.x, viewpt.y-1, double(dsm(viewpt.y-1, viewpt.x) + h)};
    for (int i = 1; i < viewpt.y; i++) {
      Vector3 target = {double(viewpt.x),
                        double(viewpt.y-1 - i),
                        double(dsm(viewpt.y-1 - i, viewpt.x) + h)};
      Vector3 updateTemp = updateLine(viewpt, target, temp, max_dis);
      if (updateTemp.z == target.z &&
          updateTemp.x == target.x &&
          updateTemp.y == target.y) {
        temp = target;
        visible(target.y, target.x) = 1;
      }
    }
  }

  // to S
  if (viewpt.y <= cols-2) {
    temp = {viewpt.x, viewpt.y+1, double(dsm(viewpt.y+1, viewpt.x) + h)};
    for (int i = 1; i < rows-viewpt.y; i++) {
      Vector3 target = {double(viewpt.x),
                        double(viewpt.y+1 + i),
                        double(dsm(viewpt.y+1 + i, viewpt.x) + h)};
      Vector3 updateTemp = updateLine(viewpt, target, temp, max_dis);
      if (updateTemp.z == target.z &&
          updateTemp.x == target.x &&
          updateTemp.y == target.y) {
        temp = target;
        visible(target.y, target.x) = 1;
      }
    }
  }

  // to E
  if (viewpt.x <= rows-2) {
    temp = {viewpt.x+1, viewpt.y, double(dsm(viewpt.y, viewpt.x+1) + h)};
    for (int i = 1; i < cols-viewpt.x; i++) {
      Vector3 target = {double(viewpt.x+1 + i),
                        double(viewpt.y),
                        double(dsm(viewpt.y, viewpt.x+1 + i) + h)};
      Vector3 updateTemp = updateLine(viewpt, target, temp, max_dis);
      if (updateTemp.z == target.z &&
          updateTemp.x == target.x &&
          updateTemp.y == target.y) {
        temp = target;
        visible(target.y, target.x) = 1;
      }
    }
  }

  // to W
  if (viewpt.x >= 2) {
    temp = {viewpt.x-1, viewpt.y, double(dsm(viewpt.y, viewpt.x-1) + h)};
    for (int i = 1; i < cols; i++) {
      Vector3 target = {double(viewpt.x-1 - i),
                        double(viewpt.y),
                        double(dsm(viewpt.y, viewpt.x-1 - i) + h)};
      Vector3 updateTemp = updateLine(viewpt, target, temp, max_dis);
      if (updateTemp.z == target.z &&
          updateTemp.x == target.x &&
          updateTemp.y == target.y) {
        temp = target;
        visible(target.y, target.x) = 1;
      }
    }
  }

  // to SW
  if (viewpt.x >= 2 && viewpt.y <= cols-2) {
    temp = {viewpt.x-1, viewpt.y+1, double(dsm(viewpt.y+1, viewpt.x-1) + h)};
    for (int i = 1; i < rows-viewpt.y; i++) {
      Vector3 target = {double(viewpt.x-1 - i),
                        double(viewpt.y+1 + i),
                        double(dsm(viewpt.y+1 + i, viewpt.x-1 - i) + h)};
      Vector3 updateTemp = updateLine(viewpt, target, temp, max_dis);
      if (updateTemp.z == target.z &&
          updateTemp.x == target.x &&
          updateTemp.y == target.y) {
        temp = target;
        visible(target.y, target.x) = 1;
      }
    }
  }

  // to NE
  if (viewpt.x <= rows-2 && viewpt.y >= 2) {
    temp = {viewpt.x+1, viewpt.y-1, double(dsm(viewpt.y-1, viewpt.x+1) + h)};
    for (int i = 1; i < viewpt.y; i++) {
      Vector3 target = {double(viewpt.x+1 + i),
                        double(viewpt.y-1 - i),
                        double(dsm(viewpt.y-1 - i, viewpt.x+1 + i) + h)};
      Vector3 updateTemp = updateLine(viewpt, target, temp, max_dis);
      if (updateTemp.z == target.z &&
          updateTemp.x == target.x &&
          updateTemp.y == target.y) {
        temp = target;
        visible(target.y, target.x) = 1;
      }
    }
  }

  // to NW
  if (viewpt.x >= 2 && viewpt.y >= 2) {
    temp = {viewpt.x-1, viewpt.y-1, double(dsm(viewpt.y-1, viewpt.x-1) + h)};
    for (int i = 1; i < viewpt.y; i++) {
      Vector3 target = {double(viewpt.x-1 - i),
                        double(viewpt.y-1 - i),
                        double(dsm(viewpt.y-1 - i, viewpt.x-1 - i) + h)};
      Vector3 updateTemp = updateLine(viewpt, target, temp, max_dis);
      if (updateTemp.z == target.z &&
          updateTemp.x == target.x &&
          updateTemp.y == target.y) {
        temp = target;
        visible(target.y, target.x) = 1;
      }
    }
  }

  // to SE
  if (viewpt.x <= rows-2 && viewpt.y <= cols-2) {
    temp = {viewpt.x+1, viewpt.y+1, double(dsm(viewpt.y+1, viewpt.x+1) + h)};
    for (int i = 1; i < rows-viewpt.y; i++) {
      Vector3 target = {double(viewpt.x+1 + i),
                        double(viewpt.y+1 + i),
                        double(dsm(viewpt.y+1 + i, viewpt.x+1 + i) + h)};
      Vector3 updateTemp = updateLine(viewpt, target, temp, max_dis);
      if (updateTemp.z == target.z &&
          updateTemp.x == target.x &&
          updateTemp.y == target.y) {
        temp = target;
        visible(target.y, target.x) = 1;
      }
    }
  }

  return visible;
}

bool is_within_bounds(double x, double y, int rows, int cols) {
  return x >= 0 && x < cols && y >= 0 && y < rows;
}

// [[Rcpp::export]]
Rcpp::IntegerMatrix reference(
    const Rcpp::NumericVector &viewpoint,
    const Rcpp::NumericMatrix &dsm,
    const double h,
    const int max_dis) {
  const int rows = dsm.rows();
  const int cols = dsm.cols();
  IntegerMatrix visible(rows, cols);

  Vector3 viewpt, n, w, e, s, nw, sw, ne, se;

  viewpt.x = viewpoint[0];
  viewpt.y = viewpoint[1];
  viewpt.z = viewpoint[2];

  visible(viewpt.y, viewpt.x) = 1;

  // Update visibility for adjacent cells
  if (viewpt.x >= 1) {
    sw = {viewpt.x-1, viewpt.y+1, dsm(viewpt.y+1, viewpt.x-1) + h};
    w = {viewpt.x-1, viewpt.y, dsm(viewpt.y, viewpt.x-1) + h};
    nw = {viewpt.x-1, viewpt.y-1, dsm(viewpt.y-1, viewpt.x-1) + h};
    if (is_within_bounds(sw.x, sw.y, rows, cols)) visible(sw.y, sw.x) = 1;
    if (is_within_bounds(w.x, w.y, rows, cols)) visible(w.y, w.x) = 1;
    if (is_within_bounds(nw.x, nw.y, rows, cols)) visible(nw.y, nw.x) = 1;
  }
  if (viewpt.x <= cols-2) {
    se = {viewpt.x+1, viewpt.y+1, dsm(viewpt.y+1, viewpt.x+1) + h};
    e = {viewpt.x+1, viewpt.y, dsm(viewpt.y, viewpt.x+1) + h};
    ne = {viewpt.x+1, viewpt.y-1, dsm(viewpt.y-1, viewpt.x+1) + h};
    if (is_within_bounds(se.x, se.y, rows, cols)) visible(se.y, se.x) = 1;
    if (is_within_bounds(e.x, e.y, rows, cols)) visible(e.y, e.x) = 1;
    if (is_within_bounds(ne.x, ne.y, rows, cols)) visible(ne.y, ne.x) = 1;
  }
  if (viewpt.y >= 1) {
    nw = {viewpt.x-1, viewpt.y-1, dsm(viewpt.y-1, viewpt.x-1) + h};
    n = {viewpt.x, viewpt.y-1, dsm(viewpt.y-1, viewpt.x) + h};
    ne = {viewpt.x+1, viewpt.y-1, dsm(viewpt.y-1, viewpt.x+1) + h};
    if (is_within_bounds(nw.x, nw.y, rows, cols)) visible(nw.y, nw.x) = 1;
    if (is_within_bounds(n.x, n.y, rows, cols)) visible(n.y, n.x) = 1;
    if (is_within_bounds(ne.x, ne.y, rows, cols)) visible(ne.y, ne.x) = 1;
  }
  if (viewpt.y <= rows-2) {
    sw = {viewpt.x-1, viewpt.y+1, dsm(viewpt.y+1, viewpt.x-1) + h};
    s = {viewpt.x, viewpt.y+1, dsm(viewpt.y+1, viewpt.x) + h};
    se = {viewpt.x+1, viewpt.y+1, dsm(viewpt.y+1, viewpt.x+1) + h};
    if (is_within_bounds(sw.x, sw.y, rows, cols)) visible(sw.y, sw.x) = 1;
    if (is_within_bounds(s.x, s.y, rows, cols)) visible(s.y, s.x) = 1;
    if (is_within_bounds(se.x, se.y, rows, cols)) visible(se.y, se.x) = 1;
  }

  // if (viewpt.x >= 1) {
  //   sw.x = viewpt.x-1;
  //   sw.y = viewpt.y+1;
  //   sw.z = dsm(sw.y, sw.x) + h;
  //   w.x = viewpt.x-1;
  //   w.y = viewpt.y;
  //   w.z = dsm(w.y, w.x) + h;
  //   nw.x = viewpt.x-1;
  //   nw.y = viewpt.y-1;
  //   nw.z = dsm(nw.y, nw.x) + h;
  //   visible(sw.y,sw.x) = 1;
  //   visible(w.y,w.x) = 1;
  //   visible(nw.y,nw.x) = 1;
  // }
  // if (viewpt.x <= cols-2) {
  //   se.x = viewpt.x+1;
  //   se.y = viewpt.y+1;
  //   se.z = dsm(se.y, se.x) + h;
  //   e.x = viewpt.x+1;
  //   e.y = viewpt.y;
  //   e.z = dsm(e.y, e.x) + h;
  //   ne.x = viewpt.x+1;
  //   ne.y = viewpt.y-1;
  //   ne.z = dsm(ne.y, ne.x) + h;
  //   visible(se.y,se.x) = 1;
  //   visible(e.y,e.x) = 1;
  //   visible(ne.y,ne.x) = 1;
  // }
  // if (viewpt.y >= 1) {
  //   nw.x = viewpt.x-1;
  //   nw.y = viewpt.y-1;
  //   nw.z = dsm(nw.y, nw.x) + h;
  //   n.x = viewpt.x;
  //   n.y = viewpt.y-1;
  //   n.z = dsm(n.y, n.x) + h;
  //   ne.x = viewpt.x+1;
  //   ne.y = viewpt.y-1;
  //   ne.z = dsm(ne.y, ne.x) + h;
  //   visible(nw.y,nw.x) = 1;
  //   visible(n.y,n.x) = 1;
  //   visible(ne.y,ne.x) = 1;
  // }
  // if (viewpt.y <= rows-2) {
  //   sw.x = viewpt.x-1;
  //   sw.y = viewpt.y+1;
  //   sw.z = dsm(sw.y, sw.x) + h;
  //   s.x = viewpt.x;
  //   s.y = viewpt.y+1;
  //   s.z = dsm(s.y, s.x) + h;
  //   se.x = viewpt.x+1;
  //   se.y = viewpt.y+1;
  //   se.z = dsm(se.y, se.x) + h;
  //   visible(sw.y,sw.x) = 1;
  //   visible(s.y,s.x) = 1;
  //   visible(se.y,se.x) = 1;
  // }

  visible = referenceLineVisible(viewpt, dsm, visible, rows, cols, max_dis, h);

  visible = wswSector(viewpt, dsm, visible, rows, max_dis, h);
  visible = wnwSector(viewpt, dsm, visible, max_dis, h);
  visible = nwnSector(viewpt, dsm, visible, max_dis, h);
  visible = nenSector(viewpt, dsm, visible, cols, max_dis, h);
  visible = eneSector(viewpt, dsm, visible, cols, max_dis, h);
  visible = sesSector(viewpt, dsm, visible, rows, cols, max_dis, h);
  visible = eseSector(viewpt, dsm, visible, rows, cols, max_dis, h);
  visible = swsSector(viewpt, dsm, visible, max_dis, rows, h);
  return visible;
}


// [[Rcpp::export]]
Rcpp::IntegerMatrix LOS(
    const Rcpp::NumericVector &viewpoint,
    const Rcpp::NumericMatrix &dsm,
    const double h,
    const int max_dis) {

  const int rows = dsm.rows();
  const int cols = dsm.cols();
  int steps;
  Rcpp::IntegerMatrix visible(rows, cols);
  Rcpp::NumericVector sequence(max_dis), xl(max_dis), yl(max_dis), zl(max_dis);

  for (int i = 0; i < rows; i++) {
     for (int j = 0; j < cols; j++) {
       steps = sqrt((viewpoint[0]-j)*(viewpoint[0]-j) + (viewpoint[1]-i)*(viewpoint[1]-i));
       const double z = dsm(i,j) + h;
       if (steps <= max_dis) {
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
