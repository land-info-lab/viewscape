#' calculate_diversity
#' @description
#'
#' @param land Raster. The raster of land use/land cover
#' @param viewshed Viewshed object [package "viewscape"].
#' @param proportion logical, indicating if the percentage(%) of each land use should
#' be returned. Default is FALSE.
#' @return Dataframe. The output is the percentage(%) of each type of land use
#' within a viewshed.
#' @export
#'
#' @references
#'
#' @examples
#'

calculate_diversity <- function(land,
                                viewshed,
                                proportion = FALSE){
  if (raster::crs(land) != viewshed@crs) {
    stop("your input rasters should have same coordinate reference system")
  }
  #pt <- filter_viewshed(viewshed)
  temp_raster <- pt %>%
    sp::SpatialPoints() %>%
    raster::raster(crs=dsm@crs, resolution=raster::res(dsm))
  # calculate the proportion of each class
  viewshed_extent <- raster::extent(temp_raster)
  land <- raster::crop(land, viewshed_extent)
  if(raster::res(land)[1] != raster::res(dsm)[1]){
    land <- raster::resample(land, temp_raster, method='ngb')
  }
  land_class <- landuse[raster::cellFromXY(land,
                                           cbind(pt$x,pt$y))]
  class_df <- data.frame(class=land_class, count=1)
  class_df <- base::subset(class_df,
                           is.na(class_df$class)==FALSE,
                           select = c(class, count))
  land_class <- dplyr::count(class_df, class)
  land_class$total <- sum(land_class$n)
  land_class$proportion <- land_class$n/land_class$total*100
  # calculate Shannon diversity index
  p <- land_class$proportion
  p <- p[p > 0]
  sdi <- sd_index(p)
  return(land_class)
}
