#' get_lidar
#' @description Search for and download LiDAR data based on coordinates
#' of a spatial point with a given distance.
#'
#' @param x numeric, indicating Longtitude of the center point.
#' @param y numeric, indicating latitude of the center point.
#' @param r numeric, indicating searching distance.
#' @param epsg numeric, indicating epsg code
#' @param folder string, indicating a path for downloading the LiDAR data
#'
#' @return .laz file

get_lidar <- function(x = NULL, y = NULL, r = NULL, epsg, folder) {
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
  result <- return_response(bbox)
  title <- result$titles
  download <- result$downloadLazURL
  for (i in 1:num) {
    destination <- paste0(folder, "/", title[i], ".laz")
    download.file(download[i], destination)
  }
}

#-83.741289,42.270146
