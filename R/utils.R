#' @noRd
#' @useDynLib viewscape
#' @import Rcpp
#' @import raster
#' @import sp
#' @import sf
#' @importFrom Rcpp sourceCpp
#'
radius_viewshed <- function(dsm, r, viewPt, offset, offset2 = 0) {
  # create an extent to crop input raster
  if(is.null(r) == FALSE){
    subarea <- get_buffer(viewPt[1], viewPt[2], r)
    subdsm <- raster::crop(dsm, raster::extent(subarea))
    dsm <- subdsm
  }
  # setup the view point
  col <- raster::colFromX(dsm, viewPt[1])
  row <- raster::rowFromY(dsm, viewPt[2])
  z_viewpoint = dsm[raster::cellFromXY(dsm,cbind(viewPt[1],viewPt[2]))]+offset
  viewpoint <- matrix(0,1,3)
  viewpoint[1,1] <- col
  viewpoint[1,2] <- row
  viewpoint[1,3] <- z_viewpoint
  # get raster information
  dsm_matrix <- raster::as.matrix(dsm)
  # compute viewshed
  output <- visibleLabel(viewpoint, dsm_matrix, offset2)
  e <- raster::extent(dsm)
  return(list(output, e))
}

filter_viewshed <- function(viewshed, extent) {
  raster_data <- raster::raster(viewshed)
  raster::extent(raster_data) <- extent
  raster::res(raster_data) <- raster::res(dsm)
  pt <- raster::rasterToPoints(raster_data)
  pt <- pt[pt[,3] == 1,]
  return(pt)
}

# H=−∑[(pi)×ln(pi)]
sd_index <- function(p) {
  return(sum(ln(p) * p) * -1)
}

# create a buffer based on a given point
get_buffer <- function(x, y, r) {
  pdf <- data.frame(row.names = 1)
  pdf[1,"x"] <- x
  pdf[1,"y"] <- y
  p <- sp::SpatialPoints(pdf)
  p <- sf::st_as_sf(p)
  subarea <- sf::st_buffer(p, r)
  return(subarea)
}
