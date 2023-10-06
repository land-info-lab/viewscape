#' calculate_diversity
#'
#' @param landuse Raster. The raster of land use/land cover
#' @param dsm Raster. The digital surface model(DSM) that is used for
#' function 'calculate_viewshed' to calculate viewshed.
#' @param visiblepoints Dataframe. The viewshed calulated by
#' function 'calculate_viewshed'.
#'
#' @return Dataframe. The output is the percentage(%) of each type of land use
#' within a viewshed.
#' @export
#'
#' @examples
calculate_diversity <- function(land, dsm, r, viewpoint, offset_viewpoint, out){
  output <- radius_viewshed(dsm, r, viewpoints, offset_viewpoint)
  raster_data <- raster::raster(output[1])
  raster::extent(raster_data) <- output[2]
  raster::res(raster_data) <- raster::res(dsm)
  #mask the raster
  tmpfilter <- raster_data == 1
  filtered_raster <- raster::mask(visibleR, tmpfilter)
  temp_raster <- visiblepoints %>%
    #create a empty raster using the extent of viewshed
    sp::SpatialPoints() %>%
    raster::raster(crs=dsm@crs, resolution=raster::res(dsm))
  #get extent of the empty raster
  viewshed_extent <- raster::extent(temp_raster)
  dsm <- raster::crop(dsm, viewshed_extent)
  landuse <- raster::crop(landuse, viewshed_extent)
  if(raster::res(landuse)[1] != raster::res(dsm)[1]){
    landuse <- raster::resample(landuse, dsm, method='ngb')
  }
  land_class <- landuse[raster::cellFromXY(landuse,
                                           cbind(visiblepoints$x,visiblepoints$y))]
  class_df <- data.frame(class=land_class, count=1)
  # remove NULL value and nodata(class 0 is nadata in the raster of land use)
  class_df <- base::subset(class_df,
                           is.na(class_df$class)==FALSE & class_df$class!=0,
                           select = c(class, count))
  land_class <- dplyr::count(class_df, class)
  land_class$total<- sum(land_class$n)
  land_class$proportion <- land_class$n/land_class$total*100
  return(land_class)
}
