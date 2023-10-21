#' An S4 class to represent the viewshed
#'
#' A `viewshed` object contains a 'matrix' of visible and invisible area,
#' resolution, extent, and crs
#' @slot visible matrix
#' @slot resolution vector
#' @slot extent Extent [package 'raster']
#' @slot crs crs [package 'raster']
#'
#' @md

setClass(
  Class = "Viewshed",
  representation(viewpoint = "numeric",
                 visible = "matrix",
                 resolution = "numeric",
                 extent = "Extent",
                 crs = "CRS")
)

setGeneric("filter_invisible", function(object, ifRaster){
  standardGeneric("filter_invisible")
})

setMethod("filter_invisible", signature(object="Viewshed", ifRaster="logical"),
          function(object, ifRaster)
          {
            raster_data <- raster::raster(object@visible)
            raster::extent(raster_data) <- object@extent
            raster::res(raster_data) <- object@resolution
            if (ifRaster) {
              return(raster_data)
            } else {
              pt <- raster::rasterToPoints(raster_data)
              pt <- pt[pt[,3] == 1,]
              pt <- pt[,-3]
              return(pt)
            }
          })
