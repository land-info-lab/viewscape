#' compute_viewshed
#' @description The compute_viewshed function is designed for computing viewsheds,
#' which are areas visible from specific viewpoints, based on a Digital Surface
#' Model (DSM). It provides flexibility for single or multi-viewpoint analyses
#' and allows options for parallel processing, raster output, and plotting.
#'
#' @param dsm Raster, the digital surface model/digital elevation model
#' @param viewpoints Vector, including x,y coordinates of a viewpoint
#' or a matrix including several viewpoints with x,y coordinates
#' (if multiviewpoints = TRUE)
#' @param offset_viewpoint numeric, setting the height of the viewpoint.
#' (default is 1.7 meters).
#' @param offset_height numeric, setting the height of positions that a given
#' viewpoint will look at. (defaut is 0)
#' @param r Numeric (optional), setting the radius for viewshed analysis.
#' (it is defaulted as NULL)
#' @param multiviewpoints the radius for viewshed analysis.
#' (it is defaulted as NULL)
#' @param parallel Logical, (default is FALSE) indicating if parallel computing
#' should be used to compute viewsheds of multiview points. When it is TRUE,
#' arguements 'raster' and 'plot' are ignored
#' @param workers Numeric, indicating the number of CPU cores that will be used
#' for parallel computing. It is required if 'parallel' is 'TRUE'.
#' @param raster Logical, (default is FALSE) if it is TRUE, the raster of
#' viewshed will be returned.
#' The default is FALSE
#' @param plot Logical, (default is FALSE) if it is TRUE, the raster of
#' viewshed will be displayed
#'
#' @return Raster or list. For single-viewpoint analysis, the function returns
#' either a raster (raster is TRUE) or a viewshed object. Value 1 means visible while
#' value 0 means invisible. For multi-viewpoint analysis, a list of viewsheds
#' is returned.
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
#'
#' # Calculate viewsheds for multiple viewpoints in parallel
#' viewsheds <- compute_viewshed(dsm, multi_viewpoints, multiviewpoints = TRUE, parallel = TRUE)
#'

compute_viewshed <- function(dsm,
                             viewpoints,
                             offset_viewpoint=1.7,
                             offset_height = 0,
                             r = NULL,
                             multiviewpoints = FALSE,
                             parallel = FALSE,
                             workers = 0,
                             raster = FALSE,
                             plot = FALSE){
  if (missing(dsm)) {
    stop("DSM is missing!")
  } else if (missing(viewpoints)) {
    stop("viewpoint(s) is missing!")
  }
  dsm_units <- sf::st_crs(dsm)$units
  if(is.null(r) == TRUE){
    if (dsm_units == "ft") {
      r <- 3281
    } else if (dsm_units == "m") {
      r <- 1000
    }
  }
  if (dsm_units == "ft" && r > 3281) {
    r <- 3281
  } else if (dsm_units == "m" && r > 1000) {
    r <- 1000
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
      if (workers == 0) {
        stop("Please specify the number of CPU cores (workers)")
      }
      if (isTRUE(Sys.info()[1]=="Windows") == FALSE){
        inputs <- split(viewpoints,seq(nrow(viewpoints)))
        bpparam <- BiocParallel::SnowParam(workers=workers, type="FORK")
        suppressWarnings(
          viewsheds <- BiocParallel::bplapply(X = inputs,
                                              FUN = radius_viewshed,
                                              dsm = dsm,
                                              r = r,
                                              offset = offset_viewpoint,
                                              BPPARAM = bpparam)
        )
      }else if (isTRUE(Sys.info()[1]=="Windows") == TRUE){
        suppressWarnings(
          viewsheds <- radius_viewshed_m(dsm=dsm,
                                         r=r,
                                         viewPts=viewpoints,
                                         offset=offset_viewpoint,
                                         workers=workers)
        )
      }
      doParallel::stopImplicitCluster()
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
