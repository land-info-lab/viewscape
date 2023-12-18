#' calculate_viewmetrics
#' @description The calculate_viewmetrics function is designed to compute a set of
#' configuration metrics based on a given viewshed object and optionally, digital surface
#' models (DSM) and digital terrain models (DTM) for terrain analysis.
#' The function calculates various metrics that describe the visibility characteristics
#' of a landscape from a specific viewpoint.The metrics include:
#' 1. Extent: The total area of the viewshed, calculated as the number of visible grid
#' cells multiplied by the grid resolution.
#' 2. Depth: The furthest visible distance within the viewshed from the viewpoint.
#' 3. Vdepth: The standard deviation of distances to visible points,
#' providing a measure of the variation in visible distances.
#' 4. Horizontal: The total visible horizontal or terrestrial area within the viewshed.
#' 5. Relief: The standard deviation of elevations of the visible ground surface.
#' 6. Skyline: The standard deviation of the vertical viewscape, including visible
#' canopy and buildings, when specified.
#' @param viewshed Viewshed object.
#' @param dsm Raster, Digital Surface Model for the calculation of
#' @param dtm Raster, Digital Terrain Model
#' @param masks List, a list including rasters of canopy and building footprints.
#' For example of canopy raster, the value for cells without canopy should be 0 and
#' the value for cells with canopy can be any number.
#' @return List
#' @references Tabrizian, P., Baran, P.K., Berkel, D.B., Mitásová, H., & Meentemeyer, R.K. (2020).
#' Modeling restorative potential of urban environments by coupling viewscape analysis of lidar
#' data with experiments in immersive virtual environments. Landscape and Urban Planning, 195, 103704.
#' @import terra
#'
#' @export

calculate_viewmetrics <- function(viewshed, dsm, dtm, masks = list()) {
  if (missing(viewshed)) {
    stop("Viewshed object is missing")
  }
  units <- sf::st_crs(viewshed@crs)$units
  if (units == "ft") {
    error <- 1.6
  } else if (units == "m") {
    error <- 0.5
  }
  if (isFALSE(terra::crs(dsm, proj = TRUE) == viewshed@crs)) {
    cat("First input dsm has different
        coordinate reference system from the viewshed\n")
    cat("Reprojetion will be processing ...\n")
    dsm <- terra::project(dsm, y=terra::crs(viewshed@crs))
  }
  if (isFALSE(terra::crs(dtm, proj = TRUE) == viewshed@crs)) {
    cat("First input dtm has different
        coordinate reference system from the viewshed\n")
    cat("Reprojetion will be processing ...\n")
    dsm <- terra::project(dtm, y=terra::crs(viewshed@crs))
  }
  output <- list()
  visiblepoints <- filter_invisible(viewshed, FALSE)
  x <- visiblepoints[,1]
  y <- visiblepoints[,2]
  pointnumber <- length(x)
  resolution <- viewshed@resolution[1]
  # extent - Total area of the viewshed
  extent <- pointnumber * resolution^2
  output[[length(output)+1]] <- extent
  # depth - Furthest visible distance given the viewscape
  depths <- get_depths(viewshed@viewpoint[1],
                       viewshed@viewpoint[2],
                       x,
                       y,
                       nrow(visiblepoints))
  # depth
  output[[length(output)+1]] <- max(depths)
  # vdepth
  output[[length(output)+1]] <- sd(depths)
  names(output) <- c("extent", "depth", "vdepth")
  dsm <- terra::crop(dsm, terra::ext(viewshed@extent, xy = TRUE))
  # horizontal - Total visible horizontal or terrestrial area
  # relief - Variation (Standard deviation) in elevation of the visible ground surface.
  if (isFALSE(missing(dsm)) && isFALSE(missing(dtm))) {
    dtm <- terra::crop(dtm, terra::ext(viewshed@extent, xy = TRUE))
    dtm_z <- terra::extract(dtm, visiblepoints)[,1]
    dsm_z <- terra::extract(dsm, visiblepoints)[,1]
    delta_z <- dsm_z - dtm_z
    z <- cbind(dtm_z, dsm_z, delta_z)
    z <- z[which(z[,3]<=error),]
    # horizontal
    output[[length(output)+1]] <- length(dsm_z) * resolution^2
    # relief
    output[[length(output)+1]] <- sd(z[,2])
    names(output) <- c("extent", "depth", "vdepth", "horizontal", "relief")
  }
  # skyline - Variation of (Standard deviation) of the vertical viewscape
  # (visible canopy and buildings)
  if (length(masks) == 2) {
    if (isFALSE(terra::crs(masks[[1]], proj = TRUE) == viewshed@crs)) {
      cat("First input mask has different
        coordinate reference system from the viewshed\n")
      cat("Reprojetion will be processing ...\n")
      masks[[1]] <- terra::project(masks[[1]], y=terra::crs(viewshed@crs))
    }
    if (isFALSE(terra::crs(masks[[2]], proj = TRUE) == viewshed@crs)) {
      cat("Second input mask has different
        coordinate reference system from the viewshed\n")
      cat("Reprojetion will be processing ...\n")
      masks[[2]] <- terra::project(masks[[2]], y=terra::crs(viewshed@crs))
    }
    masks_1 <- terra::crop(masks[[1]], terra::ext(viewshed@extent, xy = TRUE))
    masks_2 <- terra::crop(masks[[2]], terra::ext(viewshed@extent, xy = TRUE))
    dsm_z <- terra::extract(dsm, visiblepoints)[,1]
    masks_1 <- terra::extract(masks_1, visiblepoints)[,1]
    masks_2 <- terra::extract(masks_2, visiblepoints)[,1]
    mask_df <- cbind(masks_1, masks_2, masks_1+masks_2, dsm_z)
    mask_ <- mask_df[which(mask_df[,3] != 0),]
    if (length(mask_df[,4]) > 1) {
      output[[length(output)+1]] <- sd(na.omit(mask_df[,4]))
    } else {
      output[[length(output)+1]] <- 0
    }
    names(output) <- c("extent",
                       "depth",
                       "vdepth",
                       "horizontal",
                       "relief",
                       "skyline")
  }
  return(output)
}
