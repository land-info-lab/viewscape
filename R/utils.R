#' @noMd
#' @useDynLib viewscape
#' @import Rcpp
#' @import raster
#' @import sp
#' @import sf
#' @import httr2
#' @importFrom Rcpp sourceCpp

#' @noRd
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
  #return(Viewshed(output, raster::res(dsm), raster::extent(dsm)))
}

#' @noRd
filter_invisible <- function(data) {
  viewshed <- data[1]
  extent <- data[2]
  raster_data <- raster::raster(viewshed)
  raster::extent(raster_data) <- extent
  raster::res(raster_data) <- raster::res(dsm)
  pt <- raster::rasterToPoints(raster_data)
  pt <- pt[pt[,3] == 1,]
  return(pt)
}

#' @noRd
# H=−∑[(pi)×ln(pi)]
sd_index <- function(p) {
  return(sum(ln(p) * p) * -1)
}

#' @noRd
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

#' @noRd
# create a request of the TNMAccess API
return_response <- function(bbox) {
  api1 <- 'https://tnmaccess.nationalmap.gov/api/v1/products?bbox='
  api2 <- paste0(bbox[1], ",", bbox[2], ",", bbox[3], ",", bbox[4])
  api3 <- '&datasets=Lidar%20Point%20Cloud%20(LPC)&prodFormats=LAS,LAZ'
  json <- httr2::request(paste0(api1, api2, api3)) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  items <- json$total
  cat(paste0("Find ", items, " items", "\n"))
  titles <- c()
  sourceId <- c()
  metaUrl <- c()
  sizeInBytes <- c()
  startYear <- c()
  previewGraphicURL <- c()
  downloadLazURL <- c()
  if (items >= 1) {
    for (i in 1:items) {
      item <- json[[2]][[i]]
      titles <- c(titles, item$title)
      sourceId <- c(sourceId, item$sourceId)
      url <- paste0(item$metaUrl, "?format=json")
      metaUrl <- c(metaUrl, url)
      sizeInBytes <- c(sizeInBytes, item$sizeInBytes)
      startYear <- c(startYear, find_year(url))
      previewGraphicURL <- c(previewGraphicURL, item$previewGraphicURL)
      downloadLazURL <- c(downloadLazURL, item$downloadLazURL)
    }
    df <- data.frame(titles, sourceId,
                     metaUrl, sizeInBytes,
                     startYear, previewGraphicURL,
                     downloadLazURL)
    return(df)
  }
}

#' @noRd
# find year
find_year <- function(url) {
  j <- httr2::request(url) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  date <- j$dates[[2]]$dateString %>% strsplit("-") %>% unlist()
  return(as.integer(date[1]))
}

