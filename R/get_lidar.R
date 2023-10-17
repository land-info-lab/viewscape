#' get_lidar
#' @description Search for and download LiDAR data based on coordinates
#' of a spatial point with a given distance. The maximum distance is 800m.
#' Different dataset could be found and the function automatically
#' To get more details of data on a larger scale,
#' please use viewscape::lidar_search.
#'
#' @param x numeric, indicating Longtitude of the center point.
#' @param y numeric, indicating latitude of the center point.
#' @param r numeric, indicating searching distance.
#' The maximum distance is 800m (2625ft).
#' If r > 800m (2625ft), it will be reset to 800m (2625ft).
#' @param epsg numeric, indicating epsg code
#' @param folder string, indicating a path for downloading the LiDAR data
#' @param plot logical, if it is TRUE, final data will be visualized as
#' point cloud. The default is FALSE
#'
#' @return .las data
#' @references Jean-Romain Roussel and David Auty (2022).
#' Airborne LiDAR Data Manipulation and Visualization for
#' Forestry Applications. R package version 4.0.1. https://cran.r-project.org/package=lidR
#' @example
#' las <- get_lidar(-83.741289, 42.270146, 1000, 2253, 'path/to/folder')
#' raster::plot(lidR::rasterize_canopy(las, 10, dsmtin()))
#' @import sp
#' @import httr2
#' @import lidR
#' @importFrom(dplyr, "%>%")

get_lidar <- function(x,
                      y,
                      r,
                      epsg,
                      folder,
                      plot = FALSE) {
  if (missing(x) || missing(y)) {
    stop("x or y is missing. Please indicate the coordinates of centroid")
  } else if (missing(r) == TRUE) {
    stop("r is missing. Please indicate distance for searching the LiDAR data")
  } else if (missing(epsg)) {
    stop("epsg is missing. Please set epsg code")
  } else if (missing(folder)) {
    stop("folder is missng. Please set path for downloading the LiDAR data")
  }

  proj <- sp::CRS(paste0("+init=epsg:", epsg))
  longlat <- sp::CRS("+proj=longlat")
  # check searching distance
  unit <- sub(".no_defs", "", sub(".*=", "", proj@projargs))
  if (r > 800 && unit == "m ") {
    r <- 800
  } else if (r > 2625 && unit == "us-ft " ) {
    r <- 2625
  }
  # create bbox
  coor <- data.frame(lon=x, lat=y)
  pt <- sp::SpatialPoints(coor, proj4string=longlat)
  pt <- sp::spTransform(pt, proj)
  xmin <- pt@coords[1,1] - r
  xmax <- pt@coords[1,1] + r
  ymin <- pt@coords[1,2] - r
  ymax <- pt@coords[1,2] + r
  coor_ <- data.frame(lon=c(xmin, xmax), lat=c(ymin, ymax))
  pt_ <- sp::SpatialPoints(coor_, proj)
  pt_ <- sp::spTransform(pt_, CRSobj=longlat)
  bbox <- c(pt_@coords[1,1], pt_@coords[1,2], pt_@coords[2,1], pt_@coords[2,2])
  # get response using API
  result <- return_response(bbox)
  # filter overlapping files
  lastYear <- max(result$startYear)
  result <- subset(result, startYear == lastYear)
  num <- length(result[,1])
  cat(paste0("Downloading ", num," file(s)...\n"))
  title <- result$titles
  download <- result$downloadLazURL
  # download data
  files <- c()
  if (isTRUE(Sys.info()[1]=="Windows") == FALSE){
    m <- "curl"
  }else if (isTRUE(Sys.info()[1]=="Windows") == TRUE){
    m <- "wininet"
  }
  for (i in 1:num) {
    destination <- paste0(folder, "/", title[i], ".laz")
    files <- c(files, destination)
    download.file(download[i],
                  destination,
                  method = m,
                  quiet = TRUE)
  }
  # clip and merge
  lasc <- lidR::readLAScatalog(files, progress = FALSE)
  las <- lidR::clip_rectangle(lasc,
                              xleft = xmin,
                              xright = xmax,
                              ybottom = ymin,
                              ytop = ymax)
  # save
  lidR::writeLAS(las, paste0(folder, "/", Sys.time(), ".laz"))
  rm(lasc)
  # delete other laz data
  unlink(files)
  if (plot) {
    plot(las)
  }
  return(las)
}
