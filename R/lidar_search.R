#' lidar_search
#' @description Search for data within a bounding box
#' @param bbox vector, indicating the bounding box to search for data.
#'
#' @param preview logical. If TRUE, the image of the LiDAR data will
#' be display for the preview purposes. The default is FALSE
#' @param folder string, indicating a path for downloading the LiDAR data.
#' Default is NULL. If folder is set, All available data will be downloaded.
#'
#' @return dataframe
#'

lidar_search <- function(bbox, preview = FALSE, folder = NULL) {
  if (is.na(bbox) == TRUE) {
    stop("Please define a bbox")
  }
  result <- return_response(bbox)
  num <- length(result[,1])
  if (preview == TRUE) {
    url <- result$previewGraphicURL
    if (num == 1) {
      imager::load.image(url) %>% plot()
    } else {
      if (num == 2) {
        par(mfrow=c(1,2))
      } else if (num >= 3) {
        par(mfrow=c(ceiling(num/3),3))
      }
      for (i in 1:num) {
        imager::load.image(url[i]) %>% plot()
      }
    }
  }
  if (is.na(folder) == FALSE) {
    title <- result$titles
    download <- result$downloadLazURL
    for (i in 1:num) {
      destination <- paste0(folder, "/", title[i], ".laz")
      download.file(download[i], destination)
    }
  }
  return(result)
}
