#' calculate_viewshed
#'
#' @param dsm the raster layer of digital surface model/digital elevation model
#' @param viewpoints a matrix including x,y coordinates
#' or a dataframe including all viewpoints with x,y coordinates (if multiviewpoints = TRUE)
#' @param offset_viewpoint numeric, setting the height of the viewpoint.
#' @param r numeric, setting the radius for viewshed analysis. (it is defaulted as NULL)
#' @param multiviewpoints the radius for viewshed analysis. (it is defaulted as NULL)
#' @param parallel logical, indicating if parallel computing should be used to compute
#' viewsheds of multiview points.
#' @param visulization logical, indicating
#' @return Raster or matrix. The output raster/matrix is binary. Value 1 means visible
#' while value 0 means unvisible. If parallel is TRUE, the output will be a list of matrixes.
#'
#' @details Parallel computing used the functions from BiocParallel package
#'
#' @references Martin Morgan, Jiefei Wang, Valerie Obenchain, Michel Lang,
#' Ryan Thompson and Nitesh Turaga (2021). BiocParallel: Bioconductor facilities
#' for parallel evaluation. R package version 1.28.3.
#' https://github.com/Bioconductor/BiocParallel
#'
#' @export
#'
#' @examples
#'

calculate_viewshed <- function(dsm,
                               viewpoints,
                               offset_viewpoint=1.7,
                               r = NULL,
                               multiviewpoints = FALSE,
                               parallel = FALSE,
                               visulization = FALSE){
  if (multiviewpoints == FALSE){
    # compute viewshed
    output <- radius_viewshed(dsm, r, viewpoints, offset_viewpoint)
    if (visulization == TRUE) {
      raster_data <- raster::raster(output)
      raster::extent(raster_data) <- raster::extent(dsm)
      raster::res(raster_data) <- raster::res(dsm)
      raster::plot(raster_data)
      return(raster_data)
    } else {
      return(output)
    }
  }else if (multiviewpoints == TRUE){
    # set a new empty vector
    viewsheds <- c()
    if (parallel == TRUE){
      if (isTRUE(Sys.info()[1]=="Windows") == FALSE){
        type <- "FORK"
      }else if (isTRUE(Sys.info()[1]=="Windows") == TRUE){
        type <- "SOCK"
      }
      for (i in 1:length(viewpoints[,1])) {

      }
      bpparam <- BiocParallel::SnowParam(workers=parallel::detectCores(), type=type)
      suppressWarnings(
        viewsheds <- BiocParallel::bplapply(X = split(sample_points,seq(nrow(sample_points))),
                                          FUN = radius_viewshed,
                                          dsm = dsm,
                                          viewpoint = viewpoint,
                                          offset_viewpoint=offset_viewpoint,
                                          BPPARAM=bpparam)
      )
    } else {
      for(i in 1:length(viewpoints[,1])){
        viewpoint <- c(viewpoints[i,1],viewpoints[i,2])
        output <- radius_viewshed(dsm, r, viewpoint, offset_viewpoint)
        viewsheds <- c(viewsheds, list(output))
      }
    }
    return(viewsheds)
  }
}
