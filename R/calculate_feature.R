#' calculate_feature
#' @description The calculate_feature function is designed to extract specific
#' feature-related information within a viewshed. It allows you to compute
#' the proportion of the feature that is present in the viewshed.
#'
#' @param type Numeric. The input type of canopy raster.
#' type=1: percentage raster (that represents the percentage of
#' canopy area in each cell).
#' type=2: binary raster (that only uses two values to represent whether
#' canopy exists in each cell).
#' @param feature Raster.
#' @param viewshed Viewshed object.
#' @param exclude_value Numeric. If type = 2, it indicates the value of
#' cells, where the feature doesn't exist.
#'
#' @return Numeric. The canopy area in the viewshed.
#' @export
#'
#' @examples
#'

calculate_feature <- function(type,
                              feature,
                              viewshed,
                              exclude_value=0){
  if (isFALSE(terra::crs(feature, proj = TRUE) == viewshed@crs)) {
    cat("Your input (feature) rasters have different
        coordinate reference system from the viewshed\n")
    cat("Reprojetion will be processing ...\n")
    feature <- terra::project(feature, y=terra::crs(viewshed@crs))
  }
  pt <- filter_invisible(viewshed, FALSE)
  feature_df <- terra::extract(feature, pt)[,1]
  feature_ <- feature_df[feature_df!=exclude_value]
  # colnames(feature_df)[2] <- 'value'
  # feature_df <- subset(feature_df, value != exclude_value)
  if(type == 1){
    output <- sum((viewshed@resolution[1])^2*feature_)/
      (viewshed@resolution[1])^2*length(pt[,1])
  }
  else if(type == 2){
    output <- length(feature_)/length(pt[,1])
  }
  return(output)
}
