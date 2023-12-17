#' lidar_search
#' @description The lidar_search function is designed to facilitate the retrieval
#' and exploration of LiDAR (Light Detection and Ranging) data within a specified
#' bounding box (bbox). This function enables users to search for LiDAR data,
#' preview available graphics, and optionally download LiDAR data files for
#' further analysis.
#' @param bbox vector, a bounding box defining the geographical area
#' for the LiDAR data search.
#' @param max_return numeric, indicating the maximum of returns.
#' @param preview logical. If TRUE (default is FALSE), enable or disable
#' previewing LiDAR graphics.
#' @param folder string (optional), indicating an optional folder path
#' where downloaded LiDAR data files will be saved.
#'
#' @return dataframe
#'
#' @example
#' # Perform a LiDAR data search within a bounding box
#' search_result <- lidar_search(bbox, preview = TRUE, folder = "downloads")
#' # Perform a LiDAR data search and download data without preview
#' lidar_search(bbox, folder = "downloads")
#'
#' @note The lidar_search function simplifies the process of searching for
#' and working with LiDAR data via the TNMAccess API: https://tnmaccess.nationalmap.gov/api/v1/docs.
#' @export
#'
#' @example
#' bbox <- c(-83.742282,42.273389,-83.733442,42.278724)
#' search_result <- viewscape::lidar_search(bbox = bbox,
#'                                          max_return = 25,
#'                                          preview = TRUE)

lidar_search <- function(bbox,
                         max_return,
                         preview = FALSE,
                         folder = "") {
  if (missing(bbox)) {
    stop("Please define a bbox")
  }
  result <- return_response(bbox, max_return)
  num <- length(result[,1])
  if (preview == TRUE) {
    if (!requireNamespace("imager", quietly = TRUE)) {
      install.packages("imager")
    }
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
  if (isTRUE(folder != "")) {
    title <- result$titles
    download <- result$downloadLazURL
    original_timeout <- getOption('timeout')
    options(timeout=9999)
    for (i in 1:num) {
      destination <- paste0(folder, "/", title[i], ".laz")
      try(download.file(download[i], destination))
    }
    options(timeout=original_timeout)
  }
  return(result)
}
