#' calculate_diversity
#' @description
#'
#' @param land Raster. The raster of land use/land cover
#' @param viewshed Viewshed object [package "viewscape"].
#' @param proportion logical, indicating if the percentage(%) of each land use should
#' be returned. Default is FALSE.
#' @return Dataframe. The output is the percentage(%) of each type of land use
#' within a viewshed.
#' @import dplyr
#' @import raster
#' @import sp
#'
#' @export
#'
#' @references
#'
#' @examples
#'

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
  temp_raster <- pt %>%
    sp::SpatialPoints() %>%
    raster::raster(crs=viewshed@crs, resolution=viewshed@resolution)
  # calculate the proportion of each class
  land <- raster::crop(land, raster::extent(temp_raster))
  if(raster::res(land)[1] != viewshed@resolution[1]){
    land <- raster::resample(land, temp_raster, method='ngb')
  }
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
