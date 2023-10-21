#' visualize_viewshed
#'
#' @param viewshed Viewshed object
#' @param plottype the type of visualization of a viewshed,
#' including "polygon", "raster", and "3D". Default is ""
#' @param outputtype the type of visualized viewshed format to be returned,
#' including "raster" and "polygon". The default is "".
#' If it is "raster" or "polygon", the raster or polygon of a viewshed
#' will be returned
#' @return Raster or Polygon of a viewshed
#'
#' @export
#'
#' @examples
#'

visualize_viewshed <- function(viewshed,
                               plottype = "",
                               outputtype = "") {
  if (missing(viewshed)){
    stop("Viewshed object is missing")
  }
  # rasterize the viewshed
  vpt <- filter_invisible(viewshed, FALSE)
  mask_v <- raster::mask(filter_invisible(viewshed, TRUE),
                         sp::SpatialPoints(vpt))
  if (plottype == "polygon"){
    polygon_v <- raster::rasterToPolygons(mask_v)
    polygon_v <- raster::buffer(polygon_v, width = 0.0001, dissolve = TRUE)
    raster::plot(polygon_v, col = rgb(0, 1, 0, 0.3), border = NA)
  }else if (plottype == "raster"){
    raster::plot(mask_v)
  }else if (plottype == "3D"){
    if (require(rasterVis)==TRUE){
      require(rasterVis)
    }else{
      install.packages("rasterVis")
    }
    rasterVis::plot3D(mask_v)
  }
  if (outputtype == "raster" || outputtype == "polygon"){
    if (outputtype == "raster"){
      out <- mask_v
    }else if (outputtype == "polygon"){
      polygon_v <- raster::rasterToPolygons(mask_v)
      polygon_v <- raster::buffer(polygon_v, width = 0.0001, dissolve = TRUE)
      out <- polygon_v
    }
    return(out)
  }
}
