#include "iostream"
#include <Rcpp.h>
using namespace Rcpp;


// # in r
// r <- raster::raster("/Users/yangxiaohao/Downloads/GVI_DSM.tif")
// p <- raster::rasterToPoints(r)
// resolution <- min(raster::res(r))
// viewpoint <- as.vector(p[1,1:3])
// x <- p[,1]
// y <- p[,2]
// z <- p[,3]
//
// # in cpp
// viewline = sqrt( (viewpoint[1]-x[1615])^2 + (viewpoint[2]-y[1615])^2 )
//   steps <- 1 + round(viewline/ resolution)
//   xc <- viewpoint[1] + (0:steps) * (x[1615]-viewpoint[1])/steps
//   yc <- viewpoint[2] + (0:steps) * (y[1615]-viewpoint[2])/steps
//
//   mat <- matrix(, nrow = 1, ncol = 3)
//   for (j in 1:steps) {
// # xif <- abs(x-xc[j])==min(abs(x-xc[j]))
// # yif <- abs(y-yc[j])==min(abs(y-yc[j]))
// # result <- xif * yif
//     index <- which.min((x-xc[j])^2 + (y-yc[j])^2)
//     mat <- rbind(mat, c(x[index], y[index], z[index]))
//   }

//   if(viewpoint[3]<z[1615]){#when elevation of viewpoint is lower than sample
//     zl <-  viewpoint[3] + (0:steps)/steps*abs(viewpoint[3]-z[1615])
//   }else if(viewpoint[3]>z[1615]){#when elevation of viewpoint is higher than sample
//     zl <-  viewpoint[3] - (0:steps)/steps*abs(viewpoint[3]-z[1615])
//   }else if(viewpoint[3]==z[1615]){#when elevation of viewpoint equals to sample
//     zl <- viewpoint[3] + 0*(0:steps)
//   }
//   zdelta <- (zl - mat[,3])
//     zdelta <- zdelta[2:length(zdelta)]
//   if (min(zdelta) < 0) {
//     print("no")
//   }

// [[Rcpp::export]]
float numericSquare(float num) {
  return num*num ;
}

NumericVector vectorSquare(NumericVector vector) {
  return vector*vector;
}

NumericMatrix viewshedCompute(NumericVector viewpoint, NumericVector x, NumericVector y, NumericVector z, int resolution) {
  int pointNumber = x.size();
  NumericMatrix visiblePoints(pointNumber, 3);
  int counter = 0;
  for (int i = 0; i < pointNumber; i++) {
    float viewline = sqrt(numericSquare(viewpoint[1]-x[i]) + numericSquare(viewpoint[2]-y[i]));
    int steps = 1 + round(viewline/ resolution);
    NumericVector zl;
    IntegerVector sequence = seq(0, steps);
    NumericVector step = as<NumericVector>(sequence);
    NumericVector xc = viewpoint[1] + step * (x[i]-viewpoint[1])/steps;
    NumericVector yc = viewpoint[2] + step * (y[i]-viewpoint[2])/steps;
    NumericMatrix tempMatrix(steps, 3);
    for (int j = 0; j < steps; j++) {
      NumericVector distances = vectorSquare(x-xc[j]) + vectorSquare(y-yc[j]);
      int index = which_min(distances);
      NumericVector tempPt = {x[index], y[index], z[index]};
      tempMatrix.row(j) = tempPt;
    }
    if(viewpoint[3]<z[i]) {
      zl = viewpoint[3] + step/steps*fabs(viewpoint[3]-z[i]);
    } else if (viewpoint[3]>z[i]) {
      zl = viewpoint[3] - step/steps*fabs(viewpoint[3]-z[i]);
    } else if (viewpoint[3]==z[i]) {
      zl = viewpoint[3] + 0*step;
    }
    NumericVector zDelta = zl - tempMatrix(_, 3);
    IntegerVector ids = seq(0, zDelta.size()-1);
    zDelta = zDelta[ids];
    if (min(zDelta) >= 0) {
      counter ++;
      NumericVector pt = {x[i], y[i], z[i]};
      visiblePoints.row(counter-1) = pt;
    }
  }
  return visiblePoints(Range(0,counter-1), Range(0,2));
}
