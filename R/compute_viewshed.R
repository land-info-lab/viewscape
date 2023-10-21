#' compute_viewshed
#' @description Compute the viewshed based on a given spaial point or a set of points.
#' @param dsm Raster, the digital surface model/digital elevation model
#' @param viewpoints Vector, including x,y coordinates of a viewpoint
#' or a matrix including several viewpoints with x,y coordinates (if multiviewpoints = TRUE)
#' @param offset_viewpoint numeric, setting the height of the viewpoint.
#' @param offset_height numeric, setting the height of positions that a given viewpoint will
#' look at. The defaut is 0
#' @param r Numeric, setting the radius for viewshed analysis. (it is defaulted as NULL)
#' @param multiviewpoints the radius for viewshed analysis. (it is defaulted as NULL)
#' @param parallel Logical, indicating if parallel computing should be used to compute
#' viewsheds of multiview points. The default is FALSE. When it is TRUE, arguements 'raster'
#' and 'plot' are ignored
#' @param raster Logical, if it is TRUE, the raster of viewshed will be returned.
#' The default is FALSE
#' @param plot Logical, if it is TRUE, the raster of viewshed will be displayed
#'
#' @return Raster or list. If raster is TRUE, the output is a binary raster.
#' Value 1 means visible while value 0 means invisible. The list includes a binary matrix,
#' where Value 1 means visible while value 0 means invisible, and the extent of the vewshed.
#' If parallel is TRUE, the output is the list and visualization is unavailable.
#' @details Parallel computing used the functions from BiocParallel package
#'
#' @references Martin Morgan, Jiefei Wang, Valerie Obenchain, Michel Lang,
#' Ryan Thompson and Nitesh Turaga (2021). BiocParallel: Bioconductor facilities
#' for parallel evaluation. R package version 1.28.3.
#' https://github.com/Bioconductor/BiocParallel
#'
#' @useDynLib viewscape
#' @importFrom Rcpp sourceCpp
#' @importFrom raster extent
#' @importFrom raster res
#' @importFrom raster plot
#'
#' @export
#'
#' @examples
#' test_viewpoint <- sf::read_sf(system.file("test_viewpoint.shp", package = "viewscape"))
#' test_viewpoint <- sf::st_coordinates(test_viewpoint)
#' test_viewpoint <- c(test_viewpoint[,1], test_viewpoint[,2])
#' #Compute viewshed
#' dsm <- raster::raster(system.file("test_dsm.tif", package ="viewscape"))
#' output <- compute_viewshed(dsm = dsm,
#'                            viewpoints = test_viewpoint,
#'                            offset_viewpoint = 6,
#'                            raster = TRUE,
#'                            plot = TRUE)

compute_viewshed <- function(dsm,
                             viewpoints,
                             offset_viewpoint=1.7,
                             offset_height = 0,
                             r = NULL,
                             multiviewpoints = FALSE,
                             parallel = FALSE,
                             raster = FALSE,
                             plot = FALSE){
  if (missing(dsm)) {
    stop("DSM is missing!")
  } else if (missing(viewpoints)) {
    stop("viewpoint(s) is missing!")
  }
  if (plot) {
    raster <- TRUE
  }
  if (multiviewpoints == FALSE){
    # compute viewshed
    output <- radius_viewshed(dsm, r, viewpoints, offset_viewpoint, offset_height)
    if (raster) {
      raster_data <- filter_invisible(output, raster)
      if (plot) {
        raster::plot(raster_data,
                     axes=FALSE,
                     box=FALSE,
                     legend = FALSE)
        v<- matrix(0,1,3)
        v[1,1] <- viewpoints[1]
        v[1,2] <- viewpoints[2]
        raster::plot(sp::SpatialPoints(v),
                     add=TRUE,
                     col="red",
                     axes=FALSE,
                     box=FALSE,
                     legend=FALSE)
      }
      return(raster_data)
    } else {
      return(output)
    }
  }else if (multiviewpoints){
    # set a new empty vector
    viewsheds <- c()
    if (parallel == TRUE){
      if (isTRUE(Sys.info()[1]=="Windows") == FALSE){
        type <- "FORK"
      }else if (isTRUE(Sys.info()[1]=="Windows") == TRUE){
        type <- "SOCK"
      }
      bpparam <- BiocParallel::SnowParam(workers=parallel::detectCores(), type=type)
      suppressWarnings(
        viewsheds <- BiocParallel::bplapply(X = split(viewpoints,seq(nrow(viewpoints))),
                                          FUN = radius_viewshed,
                                          dsm = dsm,
                                          r = r,
                                          offset = offset_viewpoint,
                                          BPPARAM = bpparam)
      )
    } else {
      for(i in 1:length(viewpoints[,1])){
        viewpoint <- c(viewpoints[i,1],viewpoints[i,2])
        output <- radius_viewshed(dsm, r, viewpoint, offset_viewpoint)
        viewsheds <- c(viewsheds, output)
      }
    }
    return(viewsheds)
  }
}
