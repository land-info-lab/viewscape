#' visual_magnitude
#' @description Visual Magnitude quantifies the extent of a visible region
#' as perceived by an observer. It is derived from the surface's slope and
#' angle features, alongside the observer's relative distance from the area
#' in question.
#'
#' @param dsm Raster, the digital surface / elevation model
#' @param viewshed Viewshed object.
#'
#' @return SpatRaster
#'
#' @importFrom terra terrain
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
  subdsm <- terra::crop(dsm, terra::ext(viewshed@extent, xy=TRUE))
  # get slope and direction
  slope <- terra::terrain(subdsm, v="slope", neighbors=8, unit="degrees")
  direction <- terra::terrain(subdsm, v="flowdir")
  # convert raster to matrix
  dsm_matrix <- terra::as.matrix(subdsm, wide=TRUE)
  slope_matrix <- terra::as.matrix(slope, wide=TRUE)
  slope_matrix[is.nan(slope_matrix)] = 0
  direction_matrix <- terra::as.matrix(direction, wide=TRUE)
  direction_matrix[is.nan(direction_matrix)] = 0
  # compute visual magnitude
  vm_matrix <- VM(viewshed@visible, dsm_matrix,
                  slope_matrix, direction_matrix,
                  viewshed@viewpos, viewshed@viewpoint[3],
                  viewshed@resolution[1])
  viewshed@visible <- vm_matrix
  # vm raster
  vmpoints <- filter_invisible(viewshed, FALSE)
  m <- terra::vect(sp::SpatialPoints(vmpoints))
  terra::crs(m) <- viewshed@crs
  vm <- terra::mask(filter_invisible(viewshed, TRUE), m)
  return(vm)
}
