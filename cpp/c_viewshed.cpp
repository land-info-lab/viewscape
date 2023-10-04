#include "iostream"
#include <fstream>
#include <string>
#include <Rcpp.h>

using namespace Rcpp;

//[[Rcpp::export]]
int numericSquare(int num) {
  return num*num ;
}

//[[Rcpp::export]]
NumericMatrix visibleLabel(
    const NumericVector viewpoint,
    const NumericMatrix dsm,
    const int pointNumber,
    const int resolution) {

  // const int pointNumber = x.size();
  NumericVector zl;
  NumericVector xl;
  NumericVector yl;
  // //float label = Rcpp::as<float>(z);
  // float * dis = new float[pointNumber];
  // float * xa = new float[pointNumber];
  // float * ya = new float[pointNumber];
  // float * za = new float[pointNumber];//
  //double *d1 = new double[1];//
  //double *d2 = new double[1];//

  const int rows = dsm.rows();
  const int cols = dsm.cols();

  //int viewline;
  int steps;
  // float * it;
  //int index = 0;

  NumericMatrix visible(rows, cols);

  // for(int k = 0; k < pointNumber; ++k) {
  //   xa[k] = x[k];
  //   ya[k] = y[k];
  //   za[k] = z[k];
  // }

  auto start = std::chrono::system_clock::now();
  for (int i = 0; i < rows; i++) {
     for (int j = 0; j < cols; j++) {
       const double z = dsm(i,j);
       steps = sqrt(numericSquare(viewpoint[0]-j) + numericSquare(viewpoint[1]-i));
       NumericVector sequence(steps);
       std::iota(sequence.begin(), sequence.end(), 1);
       xl = viewpoint[0] + sequence * (j-viewpoint[0])/steps;
       yl = viewpoint[1] + sequence * (i-viewpoint[1])/steps;
       //Rcout << sequence[0] << "   /   ";

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
         //Rcout << d << "   ";
         if (d < 0) {
           temp = 0;
           break;
         }
       }
       visible(i,j) = temp;
     }
  }
  //for (int i = 0; i < pointNumber; i++) {
    // viewline = sqrt(numericSquare(viewpoint[0]-x[i]) + numericSquare(viewpoint[1]-y[i]));
    // steps = 1 + round(viewline/resolution);
    // NumericVector sequence(steps);
    // std::iota(sequence.begin(), sequence.end(), 0);
    // xl = viewpoint[0] + sequence * (xa[i]-viewpoint[0])/steps;
    // yl = viewpoint[1] + sequence * (ya[i]-viewpoint[1])/steps;
    //
    // if(viewpoint[2]<z[i]) {
    //   zl = viewpoint[2] + sequence/steps*fabs(viewpoint[2]-za[i]);
    // } else if (viewpoint[2]>za[i]) {
    //   zl = viewpoint[2] - sequence/steps*fabs(viewpoint[2]-za[i]);
    // } else if (viewpoint[2]==za[i]) {
    //   zl = viewpoint[2] + 0*sequence;
    // }

    //double dd[steps];
    //int temp = 1;

    //dd[0] = 1;
    // for (int j = 0; j < steps; j++) {
    //   for(int k = 0; k < pointNumber; ++k) {
    //     dis[k] = numericSquare(xa[k]-xl[j]) + numericSquare(ya[k]-yl[j]);
    //   }
    //   it = std::min_element(dis, dis + pointNumber);
    //   int index = std::distance(dis, std::find(dis, dis + pointNumber, *it));
    //   //d1[0] = zl[j] - za[index];
    //   //d2[0] = std::fabs(zl[j] - za[index]);
    //   //dd[j] = d1[0] + d2[0];
    //   float d1 = zl[j] - za[index];
    //   // double d2 = std::fabs(zl[j] - za[index]);
    //   // dd[j] = d1 + d2;
    //   // double a = dd[j];
    //   // double b = dd[j-1];
    //
    //   if (d1 < 0) {
    //     temp = 0;
    //    break;
    //   }
    //   //temp = a * b;
    //   //Rcout << temp << "   ";
    //   //temp = dd[j] * dd[j-1];//
    // }
    //const double out = temp;
    //const double out2 = 1;
    //temp = 1;
    //Rcout << temp << "   ";
    //delta.emplace_back(temp);


    // pp[i][0] = xa[i];
    // pp[i][1] = ya[i];
    // pp[i][2] = za[i];
    // pp[i][3] = temp;//
  //}

  auto end = std::chrono::system_clock::now();
  std::chrono::duration<double> elapsed_seconds = end-start;
  std::time_t end_time = std::chrono::system_clock::to_time_t(end);

  std::cout << "finished computation at " << std::ctime(&end_time)
            << "elapsed time: " << elapsed_seconds.count() << "s"
            << std::endl;
  // delete[] dis;
  // delete[] xa;
  // delete[] ya;
  // delete[] za;
  //delete[] d1;
  //delete[] d2;

  return visible;
}
