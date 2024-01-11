// #include <Rcpp.h>
// #include <cmath>
// #include <iostream>
// #include <algorithm>
// #include <thread>
// #include <memory>
// using namespace Rcpp;
// // [[Rcpp::depends(RcppParallel)]]
// #include <RcppParallel.h>
// using namespace RcppParallel;


// struct MultiLabelWorker : public RcppParallel::Worker {
// private:
//   Rcpp::NumericMatrix vpts;
//   Rcpp::List dsm;
//   const int max_dis;
//   const double vpth, h;
//   size_t startIdx, endIdx;
//   Rcpp::List localOutput;
//
// public:
//   MultiLabelWorker(const Rcpp::NumericMatrix& vptsSubset,
//                    const Rcpp::List& dsm,
//                    int max_dis,
//                    double vpth,
//                    double h)
//     : vpts(vptsSubset), dsm(dsm), max_dis(max_dis), vpth(vpth), h(h) {
//     localOutput = Rcpp::List(vptsSubset.rows());
//   }
//
//   void setRange(size_t start, size_t end) {
//     startIdx = start;
//     endIdx = end;
//   }
//
//   void operator()() {
//     // Process viewpoints from 'begin' to 'end'
//     for (std::size_t i = startIdx; i < endIdx; i++) {
//       Rcpp::NumericMatrix sub_dsm = Rcpp::as<Rcpp::NumericMatrix>(dsm[i]);
//       Rcpp::NumericVector zl;
//       Rcpp::NumericVector xl;
//       Rcpp::NumericVector yl;
//       int sub_rows = sub_dsm.rows();
//       int sub_cols = sub_dsm.cols();
//       int steps;
//       Rcpp::IntegerMatrix visible(sub_rows, sub_cols);
//       for (int j = 0; j < sub_rows; j++) {
//         for (int k = 0; k < sub_cols; k++) {
//           steps = sqrt((vpts(i,0)-k)*(vpts(i,0)-k) + (vpts(i,1)-j)*(vpts(i,1)-j));
//           if (steps <= max_dis) {
//             Rcpp::NumericVector sequence(steps);
//             std::iota(sequence.begin(), sequence.end(), 1);
//             xl = vpts(i,0) + sequence * (k-vpts(i,0))/steps;
//             yl = vpts(i,1) + sequence * (j-vpts(i,1))/steps;
//             if((vpts(i,2) + vpth)<(sub_dsm(j,k) + h)) {
//               zl = (vpts(i,2) + vpth) + sequence/steps*fabs((vpts(i,2) + vpth)-(sub_dsm(j,k) + h));
//             } else if ((vpts(i,2) + vpth)>(sub_dsm(j,k) + h)) {
//               zl = (vpts(i,2) + vpth) - sequence/steps*fabs((vpts(i,2) + vpth)-(sub_dsm(j,k) + h));
//             } else if ((vpts(i,2) + vpth)==(sub_dsm(j,k) + h)) {
//               zl = vpts(i,2) + vpth + 0*sequence;
//             }
//             int temp = 1;
//             for (int p = 0; p < steps; p++) {
//               if ((zl[p] - sub_dsm(yl[p],xl[p])) < 0) {
//                 temp = 0;
//                 break;
//               }
//             }
//             visible(j,k) = temp;
//           } else {
//             visible(j,k) = 0;
//           }
//         }
//       }
//       localOutput[i - startIdx] = visible;
//     }
//   }
//
//   Rcpp::List getResults() const {
//     return localOutput;
//   }
// };
//
// // [[Rcpp::export]]
// Rcpp::List multiLabelParallel(Rcpp::NumericMatrix& vpts,
//                               Rcpp::List& dsm,
//                               int max_dis,
//                               double vpth,
//                               double h) {
//
//   const int vptnum = vpts.rows();
//   Rcpp::List output(vptnum);
//   std::vector<MultiLabelWorker> Labelworkers;
//   int numWorkers = std::thread::hardware_concurrency();
//   int segments = vptnum / numWorkers;
//   // Create and initialize workers
//   std::vector<std::unique_ptr<MultiLabelWorker>> workers;
//   for (int i = 0; i < numWorkers; ++i) {
//     int start = i * segments;
//     int end = (i == numWorkers - 1) ? vptnum : (i + 1) * segments;
//     workers.push_back(std::make_unique<MultiLabelWorker>(vpts(Range(start, end - 1), _),
//                                        dsm,
//                                        max_dis,
//                                        vpth,
//                                        h));
//     workers[i]->setRange(start, end);
//   }
//
//   // for (size_t i = 0; i < numWorkers; ++i) {
//   //   // Execute each worker
//   //   RcppParallel::parallelFor(0, vptnum, *workers[i]);
//   // }
//   for (size_t i = 0; i < workers.size(); ++i) {
//     (*workers[i])();
//   }
//
//   // Combine results from all workers
//   for (size_t i = 0; i < Labelworkers.size(); ++i) {
//     Rcpp::List workerResults = Labelworkers[i].getResults();
//     for (size_t j = 0; j < workerResults.size(); ++j) {
//       output[i * segments + j] = workerResults[j];
//     }
//   }
//   return output;
// }
