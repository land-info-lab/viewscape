library(Rcpp)

#### test ####
dsm <- raster::raster("/Users/yangxiaohao/viewscape/inst/test_data/dsm.tif")
x_range <- raster::extent(dsm)[2] - raster::extent(dsm)[1]
y_range <- raster::extent(dsm)[4] - raster::extent(dsm)[3]
# rescope the DSM
x_min <- raster::extent(dsm)[1] + x_range/1.1
x_max <- raster::extent(dsm)[2] - x_range/1.1
y_min <- raster::extent(dsm)[3] + y_range/1.1
y_max <- raster::extent(dsm)[4] - y_range/1.1
# crop DSM
c_dsm <- raster::crop(dsm, raster::extent(c(x_min, x_max, y_min, y_max)))
# rescale DSM
s_dsm <- raster::aggregate(c_dsm, fact = 1*raster::res(c_dsm))
# compute viewpoint
row <- trunc(nrow(s_dsm)/2)
col <- trunc(ncol(s_dsm)/2)
cell <- raster::cellFromRowCol(s_dsm, row, col)
test_viewpoint <- raster::xyFromCell(s_dsm, cell)
z_viewpoint = s_dsm[raster::cellFromXY(s_dsm,cbind(test_viewpoint[1],test_viewpoint[2]))] + 6#offset_viewpoint
viewpoint <- matrix(0,1,3)
viewpoint[1,1] <- test_viewpoint[1]
viewpoint[1,2] <- test_viewpoint[2]
viewpoint[1,3] <- z_viewpoint
colnames(viewpoint) <- c('x', 'y', 'z')
# convert raster to points
sample_points <- raster::rasterToPoints(s_dsm)

sourceCpp('/Users/yangxiaohao/viewshed/viewcpp/viewscape/cpp/c_viewshed.cpp')
output <- c_viewshed(viewpoint, sample_points[,1], sample_points[,2], sample_points[,3], round(raster::res(s_dsm)[1]))

# random test
test_dsm_1000 <- spatialEco::random.raster(n.col=1000, n.row=1000,
                                           distribution="binomial")

test_dsm_1000 <- raster::raster(nrows = 1000, ncols = 1000,
                                res = 1,
                                xmn = -500, xmx = 500,
                                ymn = -500, ymx = 500,
                                vals = 1)

row <- trunc(nrow(test_dsm_1000)/2)
col <- trunc(ncol(test_dsm_1000)/2)
cell <- raster::cellFromRowCol(test_dsm_1000, row, col)
test_viewpoint <- raster::xyFromCell(test_dsm_1000, cell)
viewpoint <- matrix(0,1,3)
viewpoint[1,1] <- test_viewpoint[1]
viewpoint[1,2] <- test_viewpoint[2]
viewpoint[1,3] <- test_dsm_1000[raster::cellFromXY(test_dsm_1000,cbind(test_viewpoint[1],test_viewpoint[2]))]
colnames(viewpoint) <- c('x', 'y', 'z')
sample_points <- raster::rasterToPoints(test_dsm_1000)
output <- c_viewshed(viewpoint, sample_points[,1], sample_points[,2], sample_points[,3], round(raster::res(test_dsm_1000)[1]))
rbenchmark::benchmark("1" = {
  output <- c_viewshed(viewpoint, sample_points[,1], sample_points[,2], sample_points[,3], round(raster::res(test_dsm_1000)[1]))
  },replications = 1,
  columns = c("test", "replications", "elapsed",
            "relative", "user.self", "sys.self"))

# // # in r
#   // r <- raster::raster("/Users/yangxiaohao/Downloads/GVI_DSM.tif")
# // p <- raster::rasterToPoints(r)
# // resolution <- min(raster::res(r))
# // viewpoint <- as.vector(p[1,1:3])
# // x <- p[,1]
# // y <- p[,2]
# // z <- p[,3]
# //
#   // # in cpp
#   // viewline = sqrt( (viewpoint[1]-x[1615])^2 + (viewpoint[2]-y[1615])^2 )
# //   steps <- 1 + round(viewline/ resolution)
# //   xc <- viewpoint[1] + (0:steps) * (x[1615]-viewpoint[1])/steps
# //   yc <- viewpoint[2] + (0:steps) * (y[1615]-viewpoint[2])/steps
# //
# //   mat <- matrix(, nrow = 1, ncol = 3)
# //   for (j in 1:steps) {
#
#      index <- which.min((x-xc[j])^2 + (y-yc[j])^2)
#      mat <- rbind(mat, c(x[index], y[index], z[index]))
#     }
#
# //   if(viewpoint[3]<z[1615]){#when elevation of viewpoint is lower than sample
#   //     zl <-  viewpoint[3] + (0:steps)/steps*abs(viewpoint[3]-z[1615])
#   //   }else if(viewpoint[3]>z[1615]){#when elevation of viewpoint is higher than sample
#     //     zl <-  viewpoint[3] - (0:steps)/steps*abs(viewpoint[3]-z[1615])
#     //   }else if(viewpoint[3]==z[1615]){#when elevation of viewpoint equals to sample
#       //     zl <- viewpoint[3] + 0*(0:steps)
#       //   }
# //   zdelta <- (zl - mat[,3])
# //     zdelta <- zdelta[2:length(zdelta)]
# //   if (min(zdelta) < 0) {
#   //     print("no")
#   //   }

