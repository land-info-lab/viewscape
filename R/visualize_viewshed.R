#' visualize_viewshed
#'
#' @param viewshed the viewshed dataset of a single viewpoint from
#' function calculate viewshed
#' @param dsm the raster layer of digital surface model that has been used
#' to calcualte viewshed
#' @param plot the type of visualization of a viewshed,
#' including "polygon", "raster", and "3D".
#' @param output if TRUE, the viewshed visualization will be returned.
#' @param type the type of visualized viewshed format to be returned,
#' including "raster" and "polygon".
#' @return Raster or Polygon of a viewshed
#'
#' @export
#'
#' @examples
#'

visualize_viewshed <- function(viewshed, dsm = NULL , plot = "polygon", output = FALSE, type = NULL){
  vpt <- sp::SpatialPoints(viewshed)
  # rasterize and vectorize the viewshed
  if (is.null(dsm) == FALSE){
    mask_v <- raster::mask(dsm, vpt)
  }else{
    stop("dsm is missing")
  }
  if (plot == "polygon"){
    polygon_v <- raster::rasterToPolygons(mask_v)
    polygon_v <- raster::buffer(polygon_v, width = 0.0001, dissolve = TRUE)
    raster::plot(polygon_v, col = rgb(0, 1, 0, 0.3), border = NA)
  }else if (plot == "raster"){
    raster::plot(mask_v)
  }else if (plot == "3D"){
    if (require(rasterVis)==TRUE){
      require(rasterVis)
    }else{
      install.packages("rasterVis")
    }
    rasterVis::plot3D(mask_v)
  }
  if (output == TRUE && is.null(type) == FALSE){
    if (type == "raster"){
      return(mask_v)
    }else if ("polygon"){
      polygon_v <- raster::rasterToPolygons(mask_v)
      polygon_v <- raster::buffer(polygon_v, width = 0.0001, dissolve = TRUE)
      return(polygon_v)
    }
  }
}
