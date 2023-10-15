#' search_lidar
#'
#' @param x numeric, indicating Longtitude of the center point
#' @param y numeric, indicating latitude of the center point
#' @param r numeric, indicating searching distance
#' @param crs string, defining the coordinate systems
#' @date numeric, indicating the date of data
#' @param file string, indicating a path for downloading the LiDAR data
#'
#' @return .laz file

search_lidar <- function(x, y, crs, r, file) {
  coor <- data.frame(lon=x, lat=y)
  sp::coordinates(d) <- c("lon", "lat")

}
