#' An S4 class to represent the viewshed
#'
#' A `viewshed` object contains a 'matrix' of visible and invisible area,
#' resolution, extent, and crs
#' @slot visible matrix
#' @slot resolution vector
#' @slot extent Extent [package 'raster']
#' @slot crs crs [package 'raster']
#'

setClass(
  "Viewshed",
  slots = list(
    visible = "matrix",
    resolution = "numeric",
    extent = "Extent",
    crs = "CRS"
  )
)

mkviewshed <- function(visible, resolution, extent, crs) {
  new("Viewshed",
      visible = visible,
      resolution = resolution,
      extent = extent,
      crs = crs)
}

# setMethod("filter_invisible", signature = "Viewshed",
#           function(object)
#           {
#             raster_data <- raster::raster(object@visible)
#             raster::extent(raster_data) <- object@extent
#             raster::res(raster_data) <- raster::res(object@resolution)
#             pt <- raster::rasterToPoints(raster_data)
#             pt <- pt[pt[,3] == 1,]
#             return(pt)
#           })
