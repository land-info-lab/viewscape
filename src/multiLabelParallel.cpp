#include <Rcpp.h>
#include <cmath>
#include <iostream>
#include <algorithm>
using namespace Rcpp;
// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace RcppParallel;


struct MultiLabelWorker : public RcppParallel::Worker {
private:
  Rcpp::NumericMatrix vpts;
  Rcpp::List dsm;
  const int max_dis;
  const double vpth, h;
  Rcpp::List output;

public:
  MultiLabelWorker(Rcpp::NumericMatrix vpts,
                   Rcpp::List dsm,
                   int max_dis,
                   double vpth,
                   double h,
                   Rcpp::List &output)
    : vpts(vpts), dsm(dsm), max_dis(max_dis), vpth(vpth), h(h), output(output) {}

  void operator()(std::size_t begin, std::size_t end) {
    // Process viewpoints from 'begin' to 'end'
    for (std::size_t i = begin; i < end; i++) {
      Rcpp::NumericMatrix sub_dsm = dsm[i];
      Rcpp::NumericVector zl;
      Rcpp::NumericVector xl;
      Rcpp::NumericVector yl;
      int sub_rows = sub_dsm.rows();
      int sub_cols = sub_dsm.cols();
      int steps;
      Rcpp::IntegerMatrix visible(sub_rows, sub_cols);
      for (int j = 0; j < sub_rows; j++) {
        for (int k = 0; k < sub_cols; k++) {
          steps = sqrt((vpts(i,0)-k)*(vpts(i,0)-k) + (vpts(i,1)-j)*(vpts(i,1)-j));
          if (steps <= max_dis) {
            Rcpp::NumericVector sequence(steps);
            std::iota(sequence.begin(), sequence.end(), 1);
            xl = vpts(i,0) + sequence * (k-vpts(i,0))/steps;
            yl = vpts(i,1) + sequence * (j-vpts(i,1))/steps;
            if((vpts(i,2) + vpth)<(sub_dsm(j,k) + h)) {
              zl = (vpts(i,2) + vpth) + sequence/steps*fabs((vpts(i,2) + vpth)-(sub_dsm(j,k) + h));
            } else if ((vpts(i,2) + vpth)>(sub_dsm(j,k) + h)) {
              zl = (vpts(i,2) + vpth) - sequence/steps*fabs((vpts(i,2) + vpth)-(sub_dsm(j,k) + h));
            } else if ((vpts(i,2) + vpth)==(sub_dsm(j,k) + h)) {
              zl = vpts(i,2) + vpth + 0*sequence;
            }
            int temp = 1;
            for (int p = 0; p < steps; p++) {
              if ((zl[p] - sub_dsm(yl[p],xl[p])) < 0) {
                temp = 0;
                break;
              }
            }
            visible(j,k) = temp;
          } else {
            visible(j,k) = 0;
          }
        }
      }
      output(i) = visible;
    }
  }
};

// [[Rcpp::export]]
Rcpp::List multiLabelParallel(Rcpp::NumericMatrix& vpts,
                              Rcpp::List& dsm,
                              int max_dis,
                              double vpth,
                              double h) {

  const int vptnum = vpts.rows();
  Rcpp::List output(vptnum);

  // Create the worker
  MultiLabelWorker Label(vpts, dsm, max_dis, vpth, h, output);

  // Call parallel
  RcppParallel::parallelFor(0, vptnum, Label);

  return output;
}
