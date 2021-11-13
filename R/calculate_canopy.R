#' calculate_canopy
#'
#' @param data Numeric. The input type of canopy raster.
#' data=1: percentage raster (that represents the percentage of
#' canopy area in each cell).
#' data=2: binary raster (that only uses two values to represent whether
#' canopy exists in each cell).
#' @param canopy Raster.
#' @param nodata Numeric. If the argument 'data' is 2, indicate the value of
#' cells that don't have any canopy. (it is defaulted as NULL)
#' @param dsm Raster. The digital surface model(DSM) that is used for
#' function 'calculate_viewshed' to calculate viewshed.
#' @param visiblepoints Dataframe. The viewshed calulated by
#' function 'calculate_viewshed'.
#'
#' @return
#' @export
#'
#' @examples
calculate_canopy <- function(data, canopy, nodata=NULL, dsm, visiblepoints){
  ##data is the input type of canopy raster.
  #1=percentage raster (that represents the percentage of canopy area
  #in each cell)
  #2=binary raster (that only uses two values to represent whether canopy
  #exists in each cell)

  ##canopy is raster that only includes canopy distribution
  ##nodata is for indicateing the value of cells that don't have any canopy.
  #This is only for data=2.

  ##dsm is the DSM that is used to calculate viewshed
  ##visiblepoints is the viewshed calulated by function 'calculate_viewshed'
  ##the output is the total area or percentage of canopy within a viewshed

  #create a empty raster using the extent of viewshed
  temp_raster <- visiblepoints %>%
    sp::SpatialPoints() %>%
    raster::raster(crs=dsm@crs, resolution=raster::res(dsm))
  #get extent of the empty raster
  viewshed_extent <- raster::extent(temp_raster)
  #crop the canopy raster by the extent of the empty raster
  croped_canopy <- canopy %>% raster::crop(viewshed_extent)
  #convert croped canopy to point matrix
  canopy_points <- raster::rasterToPoints(croped_canopy)
  #convert matrix to dataframe
  canopy_df <- data.frame(x=canopy_points[,1], y=canopy_points[,2],
                          value=canopy_points[,3])
  if(data == 1){
    canopy_df$value[canopy_df$value==nodata] <- 0
    canopy_df$value <- (raster::res(canopy)[1])^2*canopy_df$value/
      max(canopy_df$value)
  }
  else if(data == 2){
    canopy_df$value <- (raster::res(canopy)[1])^2*canopy_df$value/100
  }
  sp::coordinates(canopy_df) <- ~x+y
  temp_raster <- raster::rasterize(canopy_df,
                                   temp_raster,
                                   "value", fun=function(x,...)sum(x))
  extracted_canopy <- temp_raster[raster::cellFromXY(temp_raster,
                                                  cbind(visiblepoints$x,
                                                        visiblepoints$y))]
  extracted_canopy[is.na(extracted_canopy)] <- 0
  canopy_aera <- sum(extracted_canopy)
  return(canopy_aera)
}
