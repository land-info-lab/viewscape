#' @import raster
#' @import sp
#' @import sf
#' @import httr2

#' @noMd
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
  label_matrix <- visibleLabel(viewpoint, dsm_matrix, offset2)
  output <- new("Viewshed",
                viewpoint = viewPt,
                visible = label_matrix,
                resolution = raster::res(dsm),
                extent = raster::extent(dsm),
                crs = raster::crs(dsm))
  return(output)
  #return(Viewshed(output, raster::res(dsm), raster::extent(dsm)))
}

#' @noMd
# H=−∑[(pi)×ln(pi)]
sd_index <- function(p) {
  out <- sum(log(p) * p) * -1
  return(round(out, digits = 3))
}

#' @noMd
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

#' @noMd
# create a request of the TNMAccess API
return_response <- function(bbox) {
  api1 <- 'https://tnmaccess.nationalmap.gov/api/v1/products?bbox='
  api2 <- paste0(bbox[1], ",", bbox[2], ",", bbox[3], ",", bbox[4])
  api3 <- '&datasets=Lidar%20Point%20Cloud%20(LPC)&prodFormats=LAS,LAZ'
  json <- httr2::request(paste0(api1, api2, api3)) %>% req_timeout(10) %>%
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

#' @noMd
# find year
find_year <- function(url) {
  j <- httr2::request(url) %>% req_timeout(10) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  date <- j$dates[[2]]$dateString %>% strsplit("-") %>% unlist()
  return(as.integer(date[1]))
}

