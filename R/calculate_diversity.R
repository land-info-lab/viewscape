#' calculate_diversity
#' @description The calculate_diversity function is designed to calculate landscape
#' diversity metrics within a viewshed. It takes as input a land cover raster,
#' a viewshed object representing the observer's line of sight, and an optional
#' parameter to compute class proportions.
#'
#' @param land Raster. The raster of land use/land cover representing different
#' land use/cover classes.
#' @param viewshed Viewshed object [package "viewscape"].
#' @param proportion logical (Optional), indicating whether to return class
#' proportions along with the Shannon Diversity Index (SDI). (default is FALSE).
#' @return List. a list containing the Shannon Diversity Index (SDI) and,
#' if the proportion parameter is set to TRUE, a table of class proportions
#' within the viewshed.
#' @import dplyr
#' @import raster
#' @import sp
#'
#' @export
#'
#' @references
#'
#' @examples
#' diversity_metrics <- calculate_diversity(land_cover_raster,
#'                                          viewshed_object,
#'                                          proportion = TRUE)

calculate_diversity <- function(land,
                                viewshed,
                                proportion = FALSE){
  if (isFALSE(raster::compareCRS(raster::crs(land), viewshed@crs))) {
    cat("Your input (land) rasters have different
        coordinate reference system from the viewshed\n")
    cat("Reprojetion will be processing ...\n")
    land <- raster::projectRaster(land, crs = viewshed@crs)
  }
  pt <- filter_invisible(viewshed, FALSE)
  # calculate the proportion of each class
  land_class <- raster::extract(land, pt, df=TRUE)
  colnames(land_class)[2] <- 'type'
  land_class <- dplyr::count(land_class, type)
  total <- sum(land_class$n)
  land_class$proportion <- land_class$n/total
  # calculate Shannon diversity index
  p <- land_class$proportion
  sdi <- sd_index(p)
  if (proportion) {
    return(list(SDI=sdi, Proportion=t(subset(land_class, select = c(type, proportion)))))
  } else {
    return(sdi)
  }
}
