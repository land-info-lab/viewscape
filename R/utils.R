#' @import sf
#' @importFrom graphics par
#' @importFrom grDevices rgb
#' @importFrom methods new
#' @importFrom stats sd
#' @importFrom utils download.file
#' @importFrom utils install.packages

#' @noMd
radius_viewshed <- function(dsm, r, viewPt, offset, offset2 = 0) {
  resolution <- terra::res(dsm)
  distance <- round(r/resolution[1])
  projection <- terra::crs(dsm, proj = TRUE)
  # create an extent to crop input raster
  subarea <- get_buffer(viewPt[1], viewPt[2], r)
  subdsm <- terra::crop(dsm, terra::ext(subarea))
  dsm <- subdsm
  # setup the view point
  col <- terra::colFromX(dsm, viewPt[1])
  row <- terra::rowFromY(dsm, viewPt[2])
  z_viewpoint = terra::extract(dsm,cbind(viewPt[1],viewPt[2]))[1,1]+offset
  viewpoint <- matrix(0,1,3)
  viewpoint[1,1] <- col
  viewpoint[1,2] <- row
  viewpoint[1,3] <- z_viewpoint
  # get raster information
  dsm_matrix <- terra::as.matrix(dsm, wide=TRUE)
  # compute viewshed
  label_matrix <- visibleLabel(viewpoint, dsm_matrix, offset2, distance)
  output <- new("Viewshed",
                viewpoint = viewPt,
                visible = label_matrix,
                resolution = resolution,
                extent = as.vector(sf::st_bbox(dsm)),
                crs = projection)
  return(output)
}

#' @noMd
radius_viewshed_m <- function(dsm, r, viewPts, offset, offset2 = 0) {
  output <- c()
  dsm_list <- list()
  ex <- list()
  resolution <- terra::res(dsm)
  projt <- terra::crs(dsm)
  x <- c()
  y <- c()
  z <- terra::extract(dsm, viewPts)[,1]
  distance <- round(r/resolution[1])
  for (i in 1:length(z)) {
    subarea <- get_buffer(viewPts[i,1], viewPts[i,2], r)
    subdsm <- terra::crop(dsm, terra::ext(subarea))
    e <- terra::ext(subdsm)
    ex[[i]] <- e
    x <- c(x, terra::colFromX(subdsm, viewPts[i,1]))
    y <- c(y, terra::rowFromY(subdsm, viewPts[i,2]))
    dsm_list[[i]] <- terra::as.matrix(subdsm, wide=TRUE)
  }
  vpts <- cbind(x, y)
  vpts <- cbind(vpts, z)
  label_matrix <- multiLabel(vpts=vpts,
                             dsm=dsm_list,
                             max_dis=distance,
                             vpth=offset,
                             h=offset2)
  for(i in 1:length(z)) {
    out <- new("Viewshed",
               viewpoint = viewPts[i,],
               visible = label_matrix[[i]],
               resolution = resolution,
               extent = ex[[i]],
               crs = projt)
    output <- c(output, out)
  }
  return(output)
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
# get patches
get_patch <- function(viewshed){
  vpt <- filter_invisible(viewshed, FALSE)
  m <- terra::vect(sp::SpatialPoints(vpt))
  terra::crs(m) <- viewshed@crs
  mask_ <- terra::mask(filter_invisible(viewshed, TRUE), m)
  return(mask_)
}

#' @noMd
# get patches parameter
patch_p <- function(m){
  clusters <- terra::patches(m, directions=4)
  ptc <- terra::as.polygons(clusters)
  ptc_lines <- m %>%
    terra::as.polygons() %>%
    terra::as.lines() %>%
    sf::st_as_sf()
  perimeters <- terra::perim(ptc)
  areas <- terra::expanse(ptc)
  total_perimeters <- sum(perimeters)
  total_areas <- sum(areas)
  if (sf::st_crs(m)$units == "ft") {
    num_pt <- round(total_perimeters/3.281)
  } else {
    num_pt <- round(total_perimeters)
  }
  # Number of patches
  Nump <- length(areas)
  # Mean shape index
  MSI <- mean(perimeters/areas)
  # Edge density
  ED <- total_perimeters/total_areas
  # Patch size
  PS <- total_areas/Nump
  # sample points along the edge of patches
  samples <- sf::st_sample(sf::st_cast(ptc_lines$geometry,
                                       "MULTILINESTRING"),
                           num_pt)
  samples <- sf::st_coordinates(samples)[,-3]
  return(list(Nump, MSI, ED, PS, samples))
}


#' @noMd
# create a request of the TNMAccess API
return_response <- function(bbox, max_return) {
  api1 <- 'https://tnmaccess.nationalmap.gov/api/v1/products?bbox='
  api2 <- paste0(bbox[1], ",",
                 bbox[2], ",",
                 bbox[3], ",",
                 bbox[4])
  api3 <- paste0('&datasets=Lidar%20Point%20Cloud%20(LPC)&max=',
                 max_return,
                 '&prodFormats=LAS,LAZ')
  json <- httr2::request(paste0(api1, api2, api3)) %>%
    httr2::req_timeout(10000) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  items <- length(json$items)
  cat(paste0("Get ", items, " returns", "\n"))
  cat(paste0("Find available items: ", json$total, "\n"))
  if (json$total > items) {
    cat("There are more available items\n")
    cat("You can set a greater return number to return\n")
  }
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
  j <- httr2::request(url) %>%
    httr2::req_timeout(10000) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  date <- j$dates[[2]]$dateString %>% strsplit("-") %>% unlist()
  return(as.integer(date[1]))
}

#' @noMd
paral_nix <- function(X, dsm, r, offset, workers){
  results <- pbmcapply::pbmclapply(X = X,
                                   FUN=radius_viewshed,
                                   dsm=dsm,
                                   r=r,
                                   offset=offset,
                                   mc.cores=workers)
  return(results)
}

#' @noMd
paral_win <- function(X, dsm, r, offset, workers){
  cl <- parallel::makeCluster(workers)
  results <- parallel::parLapply(cl = cl,
                                 X = seq(length(X[,1])),
                                 function(i,X,dsm,r,offset){
                                   viewpoint <- c(X[i,1],X[i,2])
                                   out <- radius_viewshed(dsm, r, viewpoint, offset)
                                   return(out)
                                 },
                                 radius_viewshed,
                                 X,dsm,r,offset
                                 )
  parallel::stopCluster(cl)
  return(results)
}
