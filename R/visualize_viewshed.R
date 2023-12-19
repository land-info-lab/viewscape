#' visualize_viewshed
#' @description The visualize_viewshed function is designed for the visualization
#' of a viewshed analysis, providing users with various options for visualizing
#' the results. The function works with a viewshed object and offers multiple
#' plotting and output types.
#'
#' @param viewshed Viewshed object
#' @param plottype String, specifying the type of visualization ("polygon,"
#' "raster," or "3D").
#' @param outputtype String, specifying the type of output object ("raster"
#' or "polygon").
#' @return Visualized viewshed as either a raster or polygon object,
#' depending on the outputtype specified.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Visualize the viewshed as polygons
#' #visualize_viewshed(viewshed, plottype = "polygon")
#' # Visualize the viewshed as a raster
#' #visualize_viewshed(viewshed, plottype = "raster")
#' # Visualize the viewshed in 3D
#' #visualize_viewshed(viewshed, plottype = "3D")
#' # Get the visualized viewshed as a polygon object
#' #polygon_viewshed <- visualize_viewshed(viewshed,
#' #                                       plottype = "polygon",
#' #                                       outputtype = "polygon")
#'}




visualize_viewshed <- function(viewshed,
                               plottype = "",
                               outputtype = "") {
  if (missing(viewshed)){
    stop("Viewshed object is missing")
  }
  # vectorize the viewshed
  mask_v <- get_patch(viewshed)
  if (plottype == "polygon"){
    polygon_v <- terra::as.polygons(mask_v)
    polygon_v <- terra::buffer(polygon_v, width = 0.0001)
    terra::plot(terra::aggregate(polygon_v), col = rgb(0, 1, 0, 0.3), border = NA)
  }else if (plottype == "raster"){
    terra::plot(mask_v)
  }else if (plottype == "3D"){
    if (require(rasterVis)==TRUE){
      require(rasterVis, quietly = TRUE)
    }else{
      install.packages("rasterVis")
    }
    rasterVis::plot3D(mask_v)
  }
  if (outputtype == "raster" || outputtype == "polygon"){
    if (outputtype == "raster"){
      out <- mask_v
    }else if (outputtype == "polygon"){
      polygon_v <- terra::as.polygons(mask_v)
      polygon_v <- terra::buffer(polygon_v, width = 0.0001)
      out <- terra::aggregate(polygon_v)
    }
    return(out)
  }
}
