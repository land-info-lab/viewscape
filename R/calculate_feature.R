#' calculate_feature
#'
#' @param type Numeric. The input type of canopy raster.
#' type=1: percentage raster (that represents the percentage of
#' canopy area in each cell).
#' type=2: binary raster (that only uses two values to represent whether
#' canopy exists in each cell).
#' @param feature Raster.
#' @param viewshed Viewshed object.
#' @param value Numeric. If type = 2, it indicates the value of
#' cells that are empty. (it is defaulted as NULL)
#'
#' @return Numeric. The canopy area in the viewshed.
#' @export
#'
#' @examples
calculate_feature <- function(type, feature, viewshed, value=NULL){
  if (raster::crs(feature) != viewshed@crs) {
    stop("your input rasters should have same coordinate reference system")
  }
  #create a empty raster using the extent of viewshed
  temp_raster <- visiblepoints %>%
    sp::SpatialPoints() %>%
    raster::raster(crs=dsm@crs, resolution=raster::res(dsm))
  #get extent of the empty raster
  viewshed_extent <- raster::extent(temp_raster)
  #crop the canopy raster by the extent of the empty raster
  croped_raster <- raster %>% raster::crop(viewshed_extent)
  #convert croped canopy to point matrix
  canopy_points <- raster::rasterToPoints(croped_canopy)
  #convert matrix to dataframe
  canopy_df <- data.frame(x=canopy_points[,1], y=canopy_points[,2],
                          value=canopy_points[,3])
  if(data == 1){
    canopy_df$value[canopy_df$value==nodata] <- 0
    canopy_df$value <- (raster::res(raster)[1])^2*canopy_df$value/
      max(canopy_df$value)
  }
  else if(data == 2){
    canopy_df$value <- (raster::res(raster)[1])^2*canopy_df$value/100
  }
  sp::coordinates(canopy_df) <- ~x+y
  temp_raster <- raster::rasterize(canopy_df,
                                   temp_raster,
                                   "value", fun=function(x,...)sum(x))
  extracted_canopy <- temp_raster[raster::cellFromXY(temp_raster,
                                                  cbind(visiblepoints$x,
                                                        visiblepoints$y))]
  extracted_canopy[is.na(extracted_canopy)] <- 0
  canopy_area <- sum(extracted_canopy)
  return(canopy_area)
}
