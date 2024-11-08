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

double curvatureRefractionAdjustment(double distance, double refraction_factor = 0.13) {
  return (distance * distance) / 12740000.0 * refraction_factor;
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

bool is_within_bounds(double x, double y, int rows, int cols) {
  return x >= 0 && x < cols && y >= 0 && y < rows;
}

Rcpp::IntegerMatrix wswSector(const Vector3 &viewpt,
                              const Rcpp::NumericMatrix &dsm,
                              Rcpp::IntegerMatrix visible,
                              int rows,
                              int max_dis,
                              const double h,
                              const double refraction_factor) {
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
      if (
          is_within_bounds(j + 1, i, dsm.nrow(), dsm.ncol()) &&
          is_within_bounds(j + 1, i - 1, dsm.nrow(), dsm.ncol()) &&
          is_within_bounds(j, i, dsm.nrow(), dsm.ncol())
      ) {
        temp_1 = {double(j + 1), double(i), referenceGrid(i, j + 1)};
        temp_2 = {double(j + 1), double(i - 1), referenceGrid(i - 1, j + 1)};
        if (sqrt((viewpt.x-j)*(viewpt.x-j) + (viewpt.y-i)*(viewpt.y-i)) <= max_dis) {

          // Calculate horizontal distance for curvature and refraction correction
          double distance = sqrt((viewpt.x - j) * (viewpt.x - j) + (viewpt.y - i) * (viewpt.y - i)) * h;
          // Adjust target height with curvature and refraction
          target = {double(j), double(i), dsm(i, j) + h - curvatureRefractionAdjustment(distance, refraction_factor)};

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
                              const double h,
                              const double refraction_factor) {
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
      if (is_within_bounds(j + 1, i, dsm.nrow(), dsm.ncol()) &&
          is_within_bounds(j + 1, i + 1, dsm.nrow(), dsm.ncol()) &&
          is_within_bounds(j, i, dsm.nrow(), dsm.ncol())) {
        temp_1 = {double(j+1), double(i), referenceGrid(double(i), double(j+1))};
        temp_2 = {double(j+1), double(i+1), referenceGrid(double(i+1), double(j+1))};
        if (sqrt((viewpt.x-j)*(viewpt.x-j) + (viewpt.y-i)*(viewpt.y-i)) <= max_dis) {
          // Calculate horizontal distance for curvature and refraction correction
          double distance = sqrt((viewpt.x - j) * (viewpt.x - j) + (viewpt.y - i) * (viewpt.y - i)) * h;
          // Adjust target height with curvature and refraction
          target = {double(j), double(i), dsm(i, j) + h - curvatureRefractionAdjustment(distance, refraction_factor)};

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

Rcpp::IntegerMatrix nwnSector(const Vector3 &viewpt,
                              const Rcpp::NumericMatrix &dsm,
                              Rcpp::IntegerMatrix visible,
                              int max_dis,
                              const double h,
                              const double refraction_factor) {
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
      if (is_within_bounds(j, i + 1, dsm.nrow(), dsm.ncol()) &&
          is_within_bounds(j + 1, i + 1, dsm.nrow(), dsm.ncol()) &&
          is_within_bounds(j, i, dsm.nrow(), dsm.ncol())) {
        temp_1 = {double(j), double(i+1), referenceGrid(double(i+1), double(j))};
        temp_2 = {double(j+1), double(i+1), referenceGrid(double(i+1), double(j+1))};
        if (sqrt((viewpt.x-j)*(viewpt.x-j) + (viewpt.y-i)*(viewpt.y-i)) <= max_dis) {

          double distance = sqrt((viewpt.x - j) * (viewpt.x - j) + (viewpt.y - i) * (viewpt.y - i)) * h;
          target = {double(j), double(i), dsm(i, j) + h - curvatureRefractionAdjustment(distance, refraction_factor)};

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

Rcpp::IntegerMatrix nenSector(const Vector3 &viewpt,
                              const Rcpp::NumericMatrix &dsm,
                              Rcpp::IntegerMatrix visible,
                              int cols,
                              int max_dis,
                              const double h,
                              const double refraction_factor) {
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
      if (is_within_bounds(j, i + 1, dsm.nrow(), dsm.ncol()) &&
          is_within_bounds(j - 1, i + 1, dsm.nrow(), dsm.ncol()) &&
          is_within_bounds(j, i, dsm.nrow(), dsm.ncol())) {
        temp_1 = {double(j), double(i+1), referenceGrid(double(i+1), double(j))};
        temp_2 = {double(j-1), double(i+1), referenceGrid(double(i+1), double(j-1))};
        if (sqrt((viewpt.x-j)*(viewpt.x-j) + (viewpt.y-i)*(viewpt.y-i)) <= max_dis) {
          // Calculate horizontal distance for curvature and refraction correction
          double distance = sqrt((viewpt.x - j) * (viewpt.x - j) + (viewpt.y - i) * (viewpt.y - i)) * h;
          // Adjust target height with curvature and refraction
          target = {double(j), double(i), dsm(i, j) + h - curvatureRefractionAdjustment(distance, refraction_factor)};

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

Rcpp::IntegerMatrix eneSector(const Vector3 &viewpt,
                              const Rcpp::NumericMatrix &dsm,
                              Rcpp::IntegerMatrix visible,
                              int cols,
                              int max_dis,
                              const double h,
                              const double refraction_factor) {
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
      if (is_within_bounds(j - 1, i, dsm.nrow(), dsm.ncol()) &&
          is_within_bounds(j - 1, i + 1, dsm.nrow(), dsm.ncol()) &&
          is_within_bounds(j, i, dsm.nrow(), dsm.ncol())) {
        temp_1 = {double(j-1), double(i), referenceGrid(double(i), double(j-1))};
        temp_2 = {double(j-1), double(i+1), referenceGrid(double(i+1), double(j-1))};
        if (sqrt((viewpt.x-j)*(viewpt.x-j) + (viewpt.y-i)*(viewpt.y-i)) <= max_dis) {

          double distance = sqrt((viewpt.x - j) * (viewpt.x - j) + (viewpt.y - i) * (viewpt.y - i)) * h;
          target = {double(j), double(i), dsm(i, j) + h - curvatureRefractionAdjustment(distance, refraction_factor)};

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

Rcpp::IntegerMatrix eseSector(const Vector3 &viewpt,
                              const Rcpp::NumericMatrix &dsm,
                              Rcpp::IntegerMatrix visible,
                              int rows,
                              int cols,
                              int max_dis,
                              const double h,
                              const double refraction_factor) {
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
      if (is_within_bounds(j - 1, i, dsm.nrow(), dsm.ncol()) &&
          is_within_bounds(j - 1, i - 1, dsm.nrow(), dsm.ncol()) &&
          is_within_bounds(j, i, dsm.nrow(), dsm.ncol())) {
        temp_1 = {double(j-1), double(i), referenceGrid(double(i), double(j-1))};
        temp_2 = {double(j-1), double(i-1), referenceGrid(double(i-1), double(j-1))};
        if (sqrt((viewpt.x-j)*(viewpt.x-j) + (viewpt.y-i)*(viewpt.y-i)) <= max_dis) {

          double distance = sqrt((viewpt.x - j) * (viewpt.x - j) + (viewpt.y - i) * (viewpt.y - i)) * h;
          target = {double(j), double(i), dsm(i, j) + h - curvatureRefractionAdjustment(distance, refraction_factor)};

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

Rcpp::IntegerMatrix sesSector(const Vector3 &viewpt,
                              const Rcpp::NumericMatrix &dsm,
                              Rcpp::IntegerMatrix visible,
                              int rows,
                              int cols,
                              int max_dis,
                              const double h,
                              const double refraction_factor) {
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

        double distance = sqrt((viewpt.x - j) * (viewpt.x - j) + (viewpt.y - i) * (viewpt.y - i)) * h;
        target = {double(j), double(i), dsm(i, j) + h - curvatureRefractionAdjustment(distance, refraction_factor)};

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
                              const double h,
                              const double refraction_factor) {
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
      if (is_within_bounds(j, i - 1, dsm.nrow(), dsm.ncol()) &&
          is_within_bounds(j - 1, i - 1, dsm.nrow(), dsm.ncol()) &&
          is_within_bounds(j, i, dsm.nrow(), dsm.ncol())) {
        temp_1 = {double(j), double(i-1), referenceGrid(double(i-1), double(j))};
        temp_2 = {double(j+1), double(i-1), referenceGrid(double(i-1), double(j+1))};
        if (sqrt((viewpt.x-j)*(viewpt.x-j) + (viewpt.y-i)*(viewpt.y-i)) <= max_dis) {

          double distance = sqrt((viewpt.x - j) * (viewpt.x - j) + (viewpt.y - i) * (viewpt.y - i)) * h;
          target = {double(j), double(i), dsm(i, j) + h - curvatureRefractionAdjustment(distance, refraction_factor)};

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
  if (distance_a > max_dis) {
    return temp;
  }

  double h_a = target.z - viewpt.z;
  double h_b = temp.z - viewpt.z;

  if (h_b < 0) {
    // Case where temp is below view height
    if (h_a >= 0 || compareTan(distance_a, distance_b, -h_a, -h_b) == 0) {
      temp = target;
    }
  } else if (h_a > 0 && compareTan(distance_a, distance_b, h_a, h_b) == 1) {
    // Case where both are above view height, and target has a higher tangent
    temp = target;
  }
  return temp;
}

// Helper function to check visibility in a given direction
void check_direction(const Vector3 &viewpt, int dx, int dy, int max_steps,
                     const Rcpp::NumericMatrix &dsm, Rcpp::IntegerMatrix &visible,
                     int max_dis, double h) {
  Vector3 temp = {viewpt.x + dx, viewpt.y + dy, dsm(viewpt.y + dy, viewpt.x + dx) + h};

  for (int i = 1; i <= max_steps; ++i) {
    int x = viewpt.x + i * dx;
    int y = viewpt.y + i * dy;

    if (!is_within_bounds(x, y, dsm.nrow(), dsm.ncol())) break;

    Vector3 target = {double(x), double(y), dsm(y, x) + h};
    Vector3 updateTemp = updateLine(viewpt, target, temp, max_dis);

    if (updateTemp.z == target.z && updateTemp.x == target.x && updateTemp.y == target.y) {
      temp = target;
      visible(y, x) = 1;
    }
  }
}

Rcpp::IntegerMatrix referenceLineVisible(const Vector3 &viewpt,
                                         const Rcpp::NumericMatrix &dsm,
                                         Rcpp::IntegerMatrix &visible,
                                         int rows, int cols, int max_dis, double h) {
  // North
  if (viewpt.y >= 2) check_direction(viewpt, 0, -1, viewpt.y, dsm, visible, max_dis, h);
  // South
  if (viewpt.y <= rows - 2) check_direction(viewpt, 0, 1, rows - viewpt.y - 1, dsm, visible, max_dis, h);
  // East
  if (viewpt.x <= cols - 2) check_direction(viewpt, 1, 0, cols - viewpt.x - 1, dsm, visible, max_dis, h);
  // West
  if (viewpt.x >= 2) check_direction(viewpt, -1, 0, viewpt.x, dsm, visible, max_dis, h);
  // Southwest
  if (viewpt.x >= 2 && viewpt.y <= rows - 2) check_direction(viewpt, -1, 1, std::min(viewpt.x, rows - viewpt.y - 1), dsm, visible, max_dis, h);
  // Northeast
  if (viewpt.x <= cols - 2 && viewpt.y >= 2) check_direction(viewpt, 1, -1, std::min(cols - viewpt.x - 1, viewpt.y), dsm, visible, max_dis, h);
  // Northwest
  if (viewpt.x >= 2 && viewpt.y >= 2) check_direction(viewpt, -1, -1, std::min(viewpt.x, viewpt.y), dsm, visible, max_dis, h);
  // Southeast
  if (viewpt.x <= cols - 2 && viewpt.y <= rows - 2) check_direction(viewpt, 1, 1, std::min(cols - viewpt.x - 1, rows - viewpt.y - 1), dsm, visible, max_dis, h);
  return visible;
}

// [[Rcpp::export]]
Rcpp::IntegerMatrix reference(
    const Rcpp::NumericVector &viewpoint,
    const Rcpp::NumericMatrix &dsm,
    const double h,
    const int max_dis,
    const double refraction_factor) {

  const int rows = dsm.rows();
  const int cols = dsm.cols();
  IntegerMatrix visible(rows, cols);

  Vector3 viewpt;

  viewpt.x = viewpoint[0];
  viewpt.y = viewpoint[1];
  viewpt.z = viewpoint[2];

  if (is_within_bounds(viewpt.y, viewpt.x, rows, cols)) {
    visible(viewpt.y, viewpt.x) = 1;
  }

  // Check and update visibility for each adjacent cell
  std::vector<Vector3> neighbors = {
    {viewpt.x - 1, viewpt.y + 1, dsm(viewpt.y + 1, viewpt.x - 1) + h}, // SW
    {viewpt.x - 1, viewpt.y, dsm(viewpt.y, viewpt.x - 1) + h},         // W
    {viewpt.x - 1, viewpt.y - 1, dsm(viewpt.y - 1, viewpt.x - 1) + h}, // NW
    {viewpt.x + 1, viewpt.y + 1, dsm(viewpt.y + 1, viewpt.x + 1) + h}, // SE
    {viewpt.x + 1, viewpt.y, dsm(viewpt.y, viewpt.x + 1) + h},         // E
    {viewpt.x + 1, viewpt.y - 1, dsm(viewpt.y - 1, viewpt.x + 1) + h}, // NE
    {viewpt.x, viewpt.y - 1, dsm(viewpt.y - 1, viewpt.x) + h},         // N
    {viewpt.x, viewpt.y + 1, dsm(viewpt.y + 1, viewpt.x) + h}          // S
  };

  for (const auto& neighbor : neighbors) {
    if (is_within_bounds(neighbor.y, neighbor.x, rows, cols)) {
      visible(neighbor.y, neighbor.x) = 1;
    }
  }

  visible = referenceLineVisible(viewpt, dsm, visible, rows, cols, max_dis, h);

  visible = wswSector(viewpt, dsm, visible, rows, max_dis, h, refraction_factor);
  visible = wnwSector(viewpt, dsm, visible, max_dis, h, refraction_factor);
  visible = nwnSector(viewpt, dsm, visible, max_dis, h, refraction_factor);
  visible = nenSector(viewpt, dsm, visible, cols, max_dis, h, refraction_factor);
  visible = eneSector(viewpt, dsm, visible, cols, max_dis, h, refraction_factor);
  visible = sesSector(viewpt, dsm, visible, rows, cols, max_dis, h, refraction_factor);
  visible = eseSector(viewpt, dsm, visible, rows, cols, max_dis, h, refraction_factor);
  visible = swsSector(viewpt, dsm, visible, max_dis, rows, h, refraction_factor);
  return visible;
}


// [[Rcpp::export]]
Rcpp::IntegerMatrix LOS(
    const Rcpp::NumericVector &viewpoint,
    const Rcpp::NumericMatrix &dsm,
    const double h,
    const int max_dis,
    const double refraction_factor) {

  const int rows = dsm.rows();
  const int cols = dsm.cols();
  int steps;
  Rcpp::IntegerMatrix visible(rows, cols);
  Rcpp::NumericVector sequence(max_dis), xl(max_dis), yl(max_dis), zl(max_dis);

  for (int i = 0; i < rows; i++) {
     for (int j = 0; j < cols; j++) {
       steps = sqrt((viewpoint[0]-j)*(viewpoint[0]-j) + (viewpoint[1]-i)*(viewpoint[1]-i));
       // Approximate horizontal distance
       double distance = steps * h;
       // Add curvature and refraction correction to target height
       double z = dsm(i,j) + h - curvatureRefractionAdjustment(distance);
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
