#' calculate_landcover
#'
#' @param landcover Raster. The raster of land cover.
#' @param dsm Raster. The digital surface model(DSM) that is used for
#' function 'calculate_viewshed' to calculate viewshed.
#' @param visiblepoints Dataframe. The viewshed calulated by
#' function 'calculate_viewshed'.
#' @param vegetation Numeric. The code of perviousness including trees or grass
#' in the raster of land cover.
#' @param imperviousness Numeric. The code of imperviousness including
#' buildings, or parkings or paths/roads in the raster of land cover.
#'
#' @return Vector. Area (sq ft) of perviousness and imperviousness. Percentages
#' of perviousness and imperviousness.
#' @export
#'
#' @examples
calculate_landcover <- function(landcover, dsm, visiblepoints,
                                vegetation, imperviousness){
  ##landcover is raster of land cover
  ##dsm is the DSM that is used to calculate viewshed
  ##visiblepoints is the viewshed calulated by function 'dsm2viewshed'
  ##vegetation and imperviousness are codes of land cover type
  #in land cover raster

  #create a empty raster using the extent of viewshed
  temp_raster <- visiblepoints %>%
    sp::SpatialPoints() %>%
    raster::raster(crs=dsm@crs, resolution=raster::res(dsm))
  #get extent of the empty raster
  viewshed_extent <- raster::extent(temp_raster)
  #crop the landcover raster by the extent of the empty raster
  croped_landcover <- landcover %>% raster::crop(viewshed_extent)
  #convert croped canopy to point matrix
  landcover_points <- raster::rasterToPoints(croped_landcover)
  #convert matrix to dataframe
  landcover_df <- data.frame(x=landcover_points[,1],
                             y=landcover_points[,2],
                             value=landcover_points[,3])
  lc_df <- landcover_df
  lc_df$value <- (raster::res(landcover)[1])^2
  sp::coordinates(lc_df) <- ~x+y
  df_temp_raster <- raster::rasterize(lc_df,
                                      temp_raster,
                                      "value", fun=function(x,...)sum(x))
  extracted_df <- df_temp_raster[rts::cellFromXY(df_temp_raster,
                                                 cbind(visiblepoints$x,
                                                       visiblepoints$y))]
  extracted_df[is.na(extracted_df)] <- 0
  vege_df <- base::subset(landcover_df, value == vegetation, select = x:value)
  vege_df$value <- (raster::res(landcover)[1])^2
  sp::coordinates(vege_df) <- ~x+y
  vege_temp_raster <- raster::rasterize(vege_df,
                                        temp_raster,
                                        "value", fun=function(x,...)sum(x))
  extracted_vege <- vege_temp_raster[rts::cellFromXY(vege_temp_raster,
                                                     cbind(visiblepoints$x,
                                                           visiblepoints$y))]
  extracted_vege[is.na(extracted_vege)] <- 0
  imper_df <- base::subset(landcover_df,
                           value == imperviousness, select = x:value)
  imper_df$value <- (raster::res(landcover)[1])^2
  sp::coordinates(imper_df) <- ~x+y
  imper_temp_raster <- raster::rasterize(imper_df,
                                         temp_raster,
                                         "value", fun=function(x,...)sum(x))
  extracted_imper <- imper_temp_raster[rts::cellFromXY(imper_temp_raster,
                                                       cbind(visiblepoints$x,
                                                             visiblepoints$y))]
  extracted_imper[is.na(extracted_imper)] <- 0
  df_area <- sum(extracted_df) #total area
  vege <- sum(extracted_vege) #area of vegetation
  imper <- sum(extracted_imper) #area of imperviousness
  vege_perc <- vege/df_area*100 #percent of vegetation
  imper_perc <- imper/df_area*100 #percent of imperviousness
  return(c(vege,vege_perc,imper,imper_perc))
}
