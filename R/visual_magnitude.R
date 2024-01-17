#' visual_magnitude
#' @description Visual Magnitude quantifies the extent of a visible region
#' as perceived by an observer. It is derived from the surface's slope and
#' angle features, alongside the observer's relative distance from the area
#' in question.
#'
#' @param dsm Raster, the digital surface / elevation model
#' @param viewshed Viewshed object.
#'
#' @return raster
#'
#' @references Chamberlain, B. C., & Meitner, M. J. (2013).
#' A route-based visibility analysis for landscape management.
#' Landscape and Urban Planning, 111, 13-24.
#'
#' @export

visual_magnitude <- function(dsm, viewshed) {
  if (missing(dsm)) {
    stop("dsm is missing")
  }
  if (missing(viewshed)) {
    stop("Viewshed object is missing")
  }
  # crop raster
  visiblepoints <- filter_invisible(viewshed, FALSE)
  m <- terra::vect(sp::SpatialPoints(visiblepoints))
  mask_ <- terra::mask(filter_invisible(viewshed, TRUE), m)
  subdsm <- terra::crop(dsm, terra::ext(mask_))
  # get slope and direction
  slope <- terra::terrain(test_dsm, v="slope", neighbors=8, unit="degrees")
  direction <- terra::terrain(subdsm, v="flowdir")
  # convert raster to matrix
  dsm_matrix <- terra::as.matrix(subdsm, wide=TRUE)
  slope_matrix <- terra::as.matrix(slope, wide=TRUE)
  direction_matrix <- terra::as.matrix(direction, wide=TRUE)
  # compute visual magnitude
  vm_matrix <- VM(viewshed@visible, dsm_matrix,
                  slope_matrix, direction_matrix,
                  viewshed@viewpos, viewshed@resolution)
  viewshed@visible <- vm_matrix
  return(filter_invisible(viewshed, TRUE))
}
