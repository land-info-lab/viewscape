#' calculate_diversity
#' @description The calculate_diversity function is designed to calculate landscape
#' diversity metrics within a viewshed. It takes as input a land cover raster,
#' a viewshed object representing the observer's line of sight, and an optional
#' parameter to compute class proportions.
#'
#' @param viewshed Viewshed object.
#' @param land Raster. The raster of land use/land cover representing different
#' land use/cover classes.
#' @param proportion logical (Optional), indicating whether to return class
#' proportions along with the Shannon Diversity Index (SDI). (default is FALSE).
#' @return List. a list containing the Shannon Diversity Index (SDI) and,
#' if the proportion parameter is set to TRUE, a table of class proportions
#' within the viewshed.
#' @import terra
#' @importFrom dplyr count
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' diversity_metrics <- calculate_diversity(viewshed_object,
#'                                          land_cover_raster)
#'}
#'

calculate_diversity <- function(viewshed,
                                land,
                                proportion = FALSE){
  if (isFALSE(terra::crs(land, proj = TRUE) == viewshed@crs)) {
    cat("Your input (land) rasters have different
        coordinate reference system from the viewshed\n")
    cat("Reprojetion will be processing ...\n")
    land <- terra::project(land, y=terra::crs(viewshed@crs))
  }
  pt <- filter_invisible(viewshed, FALSE)
  land <- terra::crop(land, terra::ext(viewshed@extent, xy = TRUE))
  # calculate the proportion of each class
  land_class <- terra::extract(land, pt)[,1]
  land_class <- as.data.frame(land_class)
  colnames(land_class)[1] <- "type"
  land_class <- dplyr::count(land_class, .data$type)
  total <- sum(land_class$n)
  land_class$proportion <- land_class$n/total
  # calculate Shannon diversity index
  p <- land_class$proportion
  sdi <- sd_index(p)
  if (proportion) {
    # sub_land_class <- subset(land_class,
    #                          select = c(type, proportion))
    sub_land_class <- land_class %>%
      dplyr::select(.data$type, proportion)
    return(list(SDI=sdi,Proportion=t(sub_land_class)))
  } else {
    return(sdi)
  }
}
