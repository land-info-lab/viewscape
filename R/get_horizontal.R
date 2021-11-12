#' get_horizontal
#'
#' @param dsm The digital surface model(DSM) that is used for
#' function 'calculate_viewshed' to calculate viewshed.
#' @param dem The raster of terrain without any vertical object
#' @param visiblepoints the viewshed calulated by function 'calculate_viewshed'.
#' @param type The metric to be returned. 1=depth(Furthest distance);
#' 2=depth variation; 3=both (a list).
#'
#' @return Numeric. If type = 1 or 2, the function will return area of
#' gound surface or standard deviation of elevations as a number. If type = 3,
#' the function will return both metrics above as a vestor.
#' @export
#'
#' @examples
get_horizontal <- function(dsm, dem, visiblepoints, type){
  ##dsm is the DSM that is used to calculate viewshed
  ##dem is the raster of terrain without any vertical object
  ##visiblepoints is the viewshed calulated by function 'dsm2viewshed'
  ##type is the metric type to be returned. 1=horizontal; 2=relief;
  #3=both (a list)

  #extract elevation from dsm to the samples along the line based on
  #corresponding coordinates
  temp_raster <- visiblepoints %>%
    #create a empty raster using the extent of viewshed
    sp::SpatialPoints() %>%
    raster::raster(crs=dsm@crs, resolution=raster::res(dsm))
  #get extent of the empty raster
  viewshed_extent <- raster::extent(temp_raster)
  dsm <- raster::crop(dsm, viewshed_extent)
  dem <- raster::crop(dem, viewshed_extent)
  if(raster::res(dsm)[1] != raster::res(dem)[1]){
    dem <- raster::resample(dem, dsm, method='ngb')
  }
  dem_z <- dem[raster::cellFromXY(dem,cbind(visiblepoints$x,visiblepoints$y))]
  dsm_z <- dsm[raster::cellFromXY(dsm,cbind(visiblepoints$x,visiblepoints$y))]
  df <- data.frame(dem_z=dem_z, dsm_z=dsm_z)
  df$delta <- df$dem_z - df$dsm_z
  horizontal_z <- base::subset(df, df$delta<=0 , select = dem_z)
  pointnumber <- length(horizontal_z$dem_z)
  resolution <- raster::res(dsm)[1]
  horizontal <- pointnumber * resolution^2
  relief <- sd(horizontal_z$dem_z)
  if(type == 1){
    return(horizontal)
  }
  else if(type == 2){
    return(relief)
  }
  else if(type == 3){
    return(c(horizontal, relief))
  }
}
