#' calculate_viewmetrics
#' @description The calculate_viewmetrics function is designed to compute a set of
#' configuration metrics based on a given viewshed object and optionally, digital surface
#' models (DSM) and digital terrain models (DTM) for terrain analysis.
#' The function calculates various metrics that describe the visibility characteristics
#' of a landscape from a specific viewpoint.The metrics include:\n
#' 1. Extent: The total area of the viewshed, calculated as the number of visible grid
#' cells multiplied by the grid resolution.\n
#' 2. Depth: The furthest visible distance within the viewshed from the viewpoint.\n
#' 3. Vdepth: The standard deviation of distances to visible points,
#' providing a measure of the variation in visible distances.\n
#' 4. Horizontal: The total visible horizontal or terrestrial area within the viewshed.\n
#' 5. Relief: The standard deviation of elevations of the visible ground surface.\n
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
#' @import raster

calculate_viewmetrics <- function(viewshed, dsm, dtm, masks = list()) {
  if (missing(viewshed)) {
    stop("Viewshed object is missing")
  }
  output <- list()
  visiblepoints <- filter_invisible(viewshed, FALSE)
  x <- visiblepoints[,1]
  y <- visiblepoints[,2]
  pointnumber <- length(x)
  resolution <- viewshed@resolution[1]
  # extent - Total area of the viewshed
  extent <- pointnumber * resolution^2
  output[[length(output)+1]] = extent
  # depth - Furthest visible distance given the viewscape
  depths <- c()
  for (i in 1:nrow(visiblepoints)) {
    distance <- sqrt((viewshed@viewpoint[1]-x[i])^2 +
                       (viewshed@viewpoint[2]-y[i])^2)
    depths <- c(depths, distance)
  }
  # depth
  output[[length(output)+1]] = max(depths)
  # vdepth
  output[[length(output)+1]] = sd(depths)
  names(output) <- c("extent", "depth", "vdepth")
  # horizontal - Total visible horizontal or terrestrial area
  # relief - Variation (Standard deviation) in elevation of the visible ground surface.
  if (isFALSE(missing(dsm)) && isFALSE(missing(dtm))) {
    dtm_z <- raster::extract(dtm, visiblepoints, df=TRUE)
    dsm_z <- raster::extract(dsm, visiblepoints, df=TRUE)
    colnames(dtm_z)[2] <- 'dtm_z'
    colnames(dsm_z)[2] <- 'dsm_z'
    z <- cbind(dtm_z, dsm_z)
    z$delta <- z$dsm_z - z$dtm_z
    z <- subset(z, delta <= 0.1)
    # horizontal
    output[[length(output)+1]] = length(z$delta) * resolution^2
    # relief
    output[[length(output)+1]] = sd(z$dtm_z)
    names(output) <- c("extent", "depth", "vdepth", "horizontal", "relief")
  }
  # skyline - Variation of (Standard deviation) of the vertical viewscape
  # (visible canopy and buildings)
  if (length(masks) == 2) {
    if (isFALSE(raster::compareCRS(raster::crs(masks[[1]]), viewshed@crs))) {
      cat("First input mask has different
        coordinate reference system from the viewshed\n")
      cat("Reprojetion will be processing ...\n")
      masks[[1]] <- raster::projectRaster(masks[[1]], crs = viewshed@crs)
    }
    if (isFALSE(raster::compareCRS(raster::crs(masks[[2]]), viewshed@crs))) {
      cat("Second input mask has different
        coordinate reference system from the viewshed\n")
      cat("Reprojetion will be processing ...\n")
      masks[[2]] <- raster::projectRaster(masks[[2]], crs = viewshed@crs)
    }
    dsm_z <- raster::extract(dsm, visiblepoints, df=TRUE)
    masks_1 <- raster::extract(masks[[1]], visiblepoints, df=TRUE)
    masks_2 <- raster::extract(masks[[2]], visiblepoints, df=TRUE)
    colnames(dsm_z)[2] <- 'z'
    colnames(masks_1)[2] <- 'masks1'
    colnames(masks_2)[2] <- 'masks2'
    mask_df <- cbind(masks_1, masks_2)
    mask_df <- cbind(mask_df, dsm_z)
    mask_df <- subset(mask_df, masks1 != 0 )
    mask_df <- subset(mask_df, masks2 != 0)
    output[[length(output)+1]] = sd(mask_df$z)
    names(output) <- c("extent", "depth", "vdepth", "horizontal", "relief", "skyline")
  }
  return(output)
}
