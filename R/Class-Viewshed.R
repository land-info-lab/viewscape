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
                 extent = "SpatExtent",
                 crs = "CRS")
)

setGeneric("filter_invisible", function(object, ifRaster){
  standardGeneric("filter_invisible")
})

setMethod("filter_invisible", signature(object="Viewshed", ifRaster="logical"),
          function(object, ifRaster)
          {
            raster_data <- terra::rast(object@visible)
            terra::ext(raster_data) <- object@extent
            terra::res(raster_data) <- object@resolution
            if (ifRaster) {
              return(raster_data)
            } else {
              pointsData <- terra::as.points(raster_data)
              coords <- terra::geom(pointsData)
              #pt <- raster::rasterToPoints(raster_data)
              x <- coords[, 1]
              y <- coords[, 2]
              z <- terra::values(pointsData)
              p <- cbind(x, y, z)
              pt <- pt[pt[,3] == 1,]
              pt <- pt[,-3]
              return(pt)
            }
          })
