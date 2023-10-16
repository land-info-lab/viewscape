#' get_lidar
#' @description Search for and download LiDAR data based on coordinates
#' of a spatial point with a given distance. The maximam distance is 800m.
#' To get data within a larger area, please use viewscape::lidar_search.
#'
#' @param x numeric, indicating Longtitude of the center point.
#' @param y numeric, indicating latitude of the center point.
#' @param r numeric, indicating searching distance.
#' @param epsg numeric, indicating epsg code
#' @param folder string, indicating a path for downloading the LiDAR data
#' @param plot string, if it is defined, final data will be visualized as
#' point cloud (plot = 'point') or raster (plot = ''). The default is NULL.
#'
#' @return .laz file
#' @references Jean-Romain Roussel and David Auty (2022).
#' Airborne LiDAR Data Manipulation and Visualization for
#' Forestry Applications. R package version 4.0.1. https://cran.r-project.org/package=lidR

get_lidar <- function(x = NULL,
                      y = NULL,
                      r = NULL,
                      epsg,
                      folder,
                      plot = NULL) {
  if (is.na(epsg) == TRUE) {
    stop("epsg is missing. Please set epsg code")
  }
  if (is.na(folder) == TRUE) {
    stop("folder is missng. Please set path for downloading the LiDAR data")
  }
  if (is.na(x) == TRUE || is.na(y) == TRUE) {
    stop("x or y is missing. Please indicate the coordinates of centroid")
  } else if (is.na(r) == TRUE) {
    stop("r is missing. Please indicate distance for searching the LiDAR data")
  }
  # create bbox
  coor <- data.frame(lon=x, lat=y)
  pt <- sp::SpatialPoints(coor, proj4string=sp::CRS("+proj=longlat"))
  pt <- sp::spTransform(pt, sp::CRS(paste0("+init=epsg:", epsg)))
  xmin <- pt@coords[1,1] - r
  xmax <- pt@coords[1,1] + r
  ymin <- pt@coords[1,2] - r
  ymax <- pt@coords[1,2] + r
  coor_ <- data.frame(lon=c(xmin, xmax), lat=c(ymin, ymax))
  pt_ <- sp::SpatialPoints(coor_, sp::CRS(paste0("+init=epsg:", epsg)))
  pt_ <- sp::spTransform(pt_, CRSobj=sp::CRS("+proj=longlat"))
  bbox <- c(pt_@coords[1,1], pt_@coords[1,2], pt_@coords[2,1], pt_@coords[2,2])
  # get response using API
  result <- return_response(bbox)
  # filter overlapping files
  lastYear <- max(result$startYear)
  result <- subset(result, startYear == lastYear)
  title <- result$titles
  download <- result$downloadLazURL
  # download data
  files <- c()
  for (i in 1:num) {
    destination <- paste0(folder, "/", title[i], ".laz")
    files <- c(files, destination)
    download.file(download[i], destination)
  }
  # clip and merge
  lasc <- lidR::readLAScatalog(files)
  las <- lidR::clip_rectangle(lasc,
                              xleft = bbox[1],
                              xright = bbox[3],
                              ybottom = bbox[2],
                              ytop = bbox[4])
  # save
  lidR::writeLAS(las, paste0(folder, "/", Sys.time(), ".laz"))
  rm(lasc)
  # delete other laz data
  unlink(files)
}

#-83.741289,42.270146
