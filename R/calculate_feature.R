#' calculate_feature
#' @description
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
  if (isFALSE(raster::compareCRS(raster::crs(feature), viewshed@crs))) {
    cat("Your input (feature) rasters have different
        coordinate reference system from the viewshed\n")
    cat("Reprojetion will be processing ...\n")
    feature <- raster::projectRaster(feature, crs = viewshed@crs)
  }
  pt <- filter_invisible(viewshed, FALSE)
  feature_df <- raster::extract(feature, pt, df=TRUE)
  colnames(feature_df)[2] <- 'value'
  feature_df <- subset(feature_df, value != exclude_value)
  if(type == 1){
    output <- sum((viewshed@resolution[1])^2*feature_df$value)/
      (viewshed@resolution[1])^2*length(pt[,1])
  }
  else if(type == 2){
    output <- length(feature_df$value)/length(pt[,1])
  }
  return(output)
}
