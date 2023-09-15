#include "iostream"
#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
float numericSquare(float num) {
  return num*num ;
}
//[[Rcpp::export]]
NumericVector vectorSquare(NumericVector vector) {
  return vector*vector;
}
//[[Rcpp::export]]
NumericMatrix c_viewshed(NumericVector viewpoint, NumericVector x, NumericVector y, NumericVector z, int resolution) {
  Rcout << "Starting computations ... ";

  int pointNumber = x.size();
  NumericMatrix visiblePoints(pointNumber, 3);

  float viewline;
  int steps;
  int index;
  int counter = 0;
  NumericVector zl;
  NumericVector xl;
  NumericVector yl;
  NumericVector xdistances(pointNumber);
  NumericVector ydistances(pointNumber);
  NumericVector distances(pointNumber);
  NumericVector tempPt(3);
  NumericVector zDelta;
  IntegerVector ids;
  NumericVector pt;

  for (int i = 0; i < pointNumber; i++) {
    //Rcout << z[i] << "/  ";
    viewline = sqrt(numericSquare(viewpoint[0]-x[i]) + numericSquare(viewpoint[1]-y[i]));
    steps = 1 + round(viewline/resolution);
    NumericVector sequence(steps);
    std::iota(sequence.begin(), sequence.end(), 0);
    xl = viewpoint[0] + sequence * (x[i]-viewpoint[0])/steps;
    yl = viewpoint[1] + sequence * (y[i]-viewpoint[1])/steps;

    if(viewpoint[2]<z[i]) {
      zl = viewpoint[2] + sequence/steps*fabs(viewpoint[2]-z[i]);
    } else if (viewpoint[2]>z[i]) {
      zl = viewpoint[2] - sequence/steps*fabs(viewpoint[2]-z[i]);
    } else if (viewpoint[2]==z[i]) {
      zl = viewpoint[2] + 0*sequence;
    }

    NumericMatrix tempMatrix(steps, 3);
    for (int j = 0; j < steps; j++) {
      xdistances = (x-xl[j])*(x-xl[j]);
      ydistances = (y-yl[j])*(y-yl[j]);
      distances = xdistances + ydistances;
      NumericVector::iterator it = std::min_element(distances.begin(),distances.end());
      index = std::distance(distances.begin(), it);
      tempPt = {x[index], y[index], z[index]};
      tempMatrix.row(j) = tempPt;
    }

    zDelta = zl - tempMatrix(_, 2);
    ids = seq(0, zDelta.size()-1);
    zDelta = zDelta[ids];
    if (min(zDelta) >= 0) {
      counter ++;
      pt = {x[i], y[i], z[i]};
      visiblePoints(counter-1 , _) = pt;
    }
  }

  return visiblePoints(Range(0,counter-1), Range(0,2));
  //return visiblePoints;
  //return 0;
}
