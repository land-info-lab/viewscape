#' @noRd
radius_viewshed <- function(dsm, r, viewPt, offset) {
  # create an extent to crop input raster
  if(is.null(r) == FALSE){
    pdf <- data.frame(row.names = 1)
    pdf[1,"x"] <- viewPt[1]
    pdf[1,"y"] <- viewPt[2]
    p <- sp::SpatialPoints(pdf)
    p <- sf::st_as_sf(p)
    subarea <- sf::st_buffer(p, r)
    subdsm <- raster::crop(dsm, raster::extent(subarea))
    dsm <- subdsm
  }
  # setup the view point
  col <- raster::colFromX(dsm, viewPt[1])
  row <- raster::rowFromY(dsm, viewPt[2])
  z_viewpoint = dsm[raster::cellFromXY(dsm,cbind(viewPt[1],viewPt[2]))] +
    offset_viewpoint
  viewpoint <- matrix(0,1,3)
  viewpoint[1,1] <- col
  viewpoint[1,2] <- row
  viewpoint[1,3] <- z_viewpoint
  # get raster information
  dsm_matrix <- raster::as.matrix(dsm)
  n_cell <- raster::ncell(dsm)
  resolution <- round(raster::res(dsm)[1])
  # compute viewshed
  output <- visibleLabel(viewpoint, dsm_matrix, n_cell, resolution)
  e <- raster::extent(dsm)
  return(c(output, e))
}

filter_viewshed <- function(viewshed, extent) {

}
