#' @noRd
#' @useDynLib viewscape
#' @import Rcpp
#' @import raster
#' @import sp
#' @import sf
#' @importFrom Rcpp sourceCpp
#'
radius_viewshed <- function(dsm, r, viewPt, offset, offset2 = 0) {
  # create an extent to crop input raster
  if(is.null(r) == FALSE){
    subarea <- get_buffer(viewPt[1], viewPt[2], r)
    subdsm <- raster::crop(dsm, raster::extent(subarea))
    dsm <- subdsm
  }
  # setup the view point
  col <- raster::colFromX(dsm, viewPt[1])
  row <- raster::rowFromY(dsm, viewPt[2])
  z_viewpoint = dsm[raster::cellFromXY(dsm,cbind(viewPt[1],viewPt[2]))]+offset
  viewpoint <- matrix(0,1,3)
  viewpoint[1,1] <- col
  viewpoint[1,2] <- row
  viewpoint[1,3] <- z_viewpoint
  # get raster information
  dsm_matrix <- raster::as.matrix(dsm)
  # compute viewshed
  output <- visibleLabel(viewpoint, dsm_matrix, offset2)
  e <- raster::extent(dsm)
  return(list(output, e))
}

filter_viewshed <- function(viewshed, extent) {
  raster_data <- raster::raster(viewshed)
  raster::extent(raster_data) <- extent
  raster::res(raster_data) <- raster::res(dsm)
  pt <- raster::rasterToPoints(raster_data)
  pt <- pt[pt[,3] == 1,]
  return(pt)
}

# H=−∑[(pi)×ln(pi)]
sd_index <- function(p) {
  return(sum(ln(p) * p) * -1)
}

# create a buffer based on a given point
get_buffer <- function(x, y, r) {
  pdf <- data.frame(row.names = 1)
  pdf[1,"x"] <- x
  pdf[1,"y"] <- y
  p <- sp::SpatialPoints(pdf)
  p <- sf::st_as_sf(p)
  subarea <- sf::st_buffer(p, r)
  return(subarea)
}

# create a request of the TNMAccess API
return_response <- function(bbox) {
  api1 <- 'https://tnmaccess.nationalmap.gov/api/v1/products?bbox='
  api2 <- paste0(bbox[1], ",", bbox[2], ",", bbox[3], ",", bbox[4])
  api3 <- '&datasets=Lidar%20Point%20Cloud%20(LPC)&prodFormats=LAS,LAZ'
  resp <- httr2::request(paste0(api1, api2, api3)) %>% req_perform()
  json <- httr2::resp_body_json(resp)

  items <- json$total
  cat(paste0("Find ", items, " items", "\n"))
  titles <- c()
  sourceId <- c()
  metaUrl <- c()
  sizeInBytes <- c()
  lastUpdated <- c()
  previewGraphicURL <- c()
  downloadLazURL <- c()
  boundingBoxMinX <- c()
  boundingBoxMaxX <- c()
  boundingBoxMinY <- c()
  boundingBoxMaxY <- c()
  if (items >= 1) {
    for (i in 1:items) {
      item <- json[[i]]
      titles <- c(titles, item$title)
      sourceId <- c(sourceId, item$sourceId)
      metaUrl <- c(metaUrl, item$metaUrl)
      sizeInBytes <- c(sizeInBytes, item$sizeInBytes)
      lastUpdated <- c(lastUpdated, item$lastUpdated)
      previewGraphicURL <- c(previewGraphicURL, item$previewGraphicURL)
      downloadLazURL <- c(downloadLazURL, item$downloadLazURL)
      boundingBoxMinX <- c(boundingBoxMinX, item$boundingBoxMinX)
      boundingBoxMaxX <- c(boundingBoxMaxX, item$boundingBoxMaxX)
      boundingBoxMinY <- c(boundingBoxMinY, item$boundingBoxMinY)
      boundingBoxMaxY <- c(boundingBoxMaxY, item$boundingBoxMaxY)
    }
    df <- data.frame(titles, sourceId,
                     metaUrl, sizeInBytes,
                     lastUpdated, previewGraphicURL,
                     downloadLazURL, boundingBoxMinX,
                     boundingBoxMaxX, boundingBoxMinY,
                     boundingBoxMaxY)
    return(df)
  }
}
