#include <Rcpp.h>
#include <iostream>
#include <fstream>
#ifdef _WIN32
#include <omp.h>  // Include OpenMP for Windows
#endif

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List multiLabel(Rcpp::NumericMatrix &vpts,
                      Rcpp::NumericMatrix &dsm,
                      const int max_dis,
                      const double vpth,
                      const double h,
                      const int workers) {
  // #ifdef _WIN32
  // omp_set_num_threads(workers);
  // #endif
  const int vptnum = vpts.rows();
  Rcpp::List output(vptnum);
  const int rows = dsm.rows();
  const int cols = dsm.cols();
  #ifdef _WIN32
  #pragma omp parallel for
  #endif
  for (int i = 0; i < vptnum; i++) {
    int vx = vpts(i,0);
    int vy = vpts(i,1);
    const double vz = vpts(i,2) + vpth;
    int xmax = vx + max_dis;
    int xmin = vx - max_dis;
    int ymax = vy + max_dis;
    int ymin = vy - max_dis;
    vx = max_dis + 1;
    vy = max_dis + 1;
    if (xmax > cols) {
      xmax = cols;
    }
    if (xmin < 0) {
      xmin = 0;
      vx = vpts(i,0);
    }
    if (ymax > rows) {
      ymax = rows;
    }
    if (ymin < 0) {
      ymin = 0;
      vy = vpts(i,1);
    }
    Rcpp::NumericMatrix sub_dsm = dsm(Range(ymin,ymax),
                                      Range(xmin,xmax));
    Rcpp::NumericVector zl;
    Rcpp::NumericVector xl;
    Rcpp::NumericVector yl;
    const int sub_rows = sub_dsm.rows();
    const int sub_cols = sub_dsm.cols();
    int steps;
    Rcpp::IntegerMatrix visible(sub_rows, sub_cols);
    for (int j = 0; j < sub_rows; j++) {
      for (int k = 0; k < sub_cols; k++) {
        steps = sqrt((vx-k)*(vx-k) + (vy-j)*(vy-j));
        const double z = dsm(j,k) + h;
        if (steps <= max_dis) {
          Rcpp::NumericVector sequence(steps);
          std::iota(sequence.begin(), sequence.end(), 1);
          xl = vx + sequence * (k-vx)/steps;
          yl = vy + sequence * (j-vy)/steps;

          if(vz<z) {
            zl = vz + sequence/steps*fabs(vz-z);
          } else if (vz>z) {
            zl = vz - sequence/steps*fabs(vz-z);
          } else if (vz==z) {
            zl = vz + 0*sequence;
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
    output(i) = visible;
  }
  return output;
}


