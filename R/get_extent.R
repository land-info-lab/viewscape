#' get_extent
#'
#' @param dsm The digital surface model(DSM) that is used for
#' function 'calculate_viewshed' to calculate viewshed.
#' @param visiblepoints the viewshed calulated by function 'calculate_viewshed'.
#'
#' @return Numeric. The area of viewshed extent.
#' @export
#'
#' @examples
get_extent <- function(dsm, visiblepoints){
  #dsm is the DSM that is used to calculate viewshed
  #visiblepoints is the viewshed calulated by function 'calculate_viewshed'

  pointnumber <- length(visiblepoints$x)
  resolution <- raster::res(dsm)[1]
  extent <- pointnumber * resolution^2
  return(extent)
}
