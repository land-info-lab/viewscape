#' calculate_viewshed
#'
#' @param dsm the raster layer of digital surface model/digital elevation model
#' @param under the raster layer of digital surface model where the elevation
#' above crown base of trees was removed
#' @param dem the raster of terrain without any vertical object. If input of
#' dsm is already digital elevation model, this argument can be ignored
#' @param viewpoint a matrix including x,y coordinates
#' or a dataframe including all viewpoints with x,y coordinates (if multiviewpoints = TRUE)
#' @param offset_viewpoint the height offset at the viewpoint. the unit of input should be meter.
#' @param offset_samples the height offset at all sample points. the unit of input should be meter
#' sample_points a matrix of points that are converted from DSM.
#' This can be ignored. (it is defaulted as NULL)
#' @param r the radius for viewshed analysis. (it is defaulted as NULL)
#' @param multiviewpoints the radius for viewshed analysis. (it is defaulted as NULL)
#'
#' @return Dataframe or list. The output dataframe includes x anf y coordinates of visible
#' locations of a single viewpoint. The output list includes x anf y coordinates of visible
#' locations of each input viewpoints.
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

calculate_viewshed <- function(dsm, under=NULL, dem=NULL,
                                     viewpoints, offset_viewpoint=1.7,
                                     offset_samples=0, sample_points=NULL,
                                     r = NULL,
                                     multiviewpoints = FALSE){
  # calculate viewshed based on a certain viewpoint

  # dsm is raster layer of digital surface model/digital elevation model
  ## under is raster layer of digital surface model where the elevation
  #above crown base of trees was removed
  ## dem is the raster of terrain without any vertical object. If input
  #of dsm is already digital elevation model, this arguement can be ignored
  # viewpoint is a matrix including x,y coordinates
  # offset_viewpoint is the height of viewpoint. the unit of input should be meter
  # offset_samples is the height of sample coordinates. the unit of input should be meter
  ## sample_points is a matrix of points that are converted from DSM.
  #This can be ignored. (it is defaulted as NULL)
  # r is the radius for viewshed analysis. (it is defaulted as NULL)

  if (require(BiocManager)==TRUE){
    require(BiocManager)
  }else{
    install.packages("BiocManager")
  }

  if (require(BiocParallel)==TRUE){
    require(BiocParallel)
  }else{
    install.packages("BiocParallel")
  }

  if (require(parallel)==TRUE){
    require(parallel)
  }else{
    install.packages("parallel")
  }

  if (multiviewpoints == FALSE){
    start_time <- Sys.time()
    if(is.null(r) == TRUE){
      if(is.null(sample_points) == TRUE){
        sample_points <- raster::rasterToPoints(dsm) #convert raster to matrix
      }
    }else{
      # create an extent to crop input raster
      pdf <- data.frame(row.names = 1)
      pdf[1,"x"] <- viewpoints[1]
      pdf[1,"y"] <- viewpoints[2]
      p <- sp::SpatialPoints(pdf)
      p <- sf::st_as_sf(p)
      subarea <- sf::st_buffer(p, r)
      subdsm <- raster::crop(dsm, raster::extent(subarea))
      dsm <- subdsm
      if(is.null(under) == FALSE){
        under <- raster::crop(under, raster::extent(subarea))
      }
      if(is.null(sample_points) == TRUE|is.null(sample_points) == FALSE){
        sample_points <- raster::rasterToPoints(subdsm) #convert raster to matrix
      }
    }

    bpparam <- BiocParallel::SnowParam(workers=detectCores(), type="FORK")
    visible_coordinates <- BiocParallel::bplapply(X = split(sample_points,seq(nrow(sample_points))),
                                                  FUN = visiblesample,
                                                  dsm = dsm, modified_dsm=under,
                                                  dem=dem, viewpoint = viewpoints,
                                                  offset_viewpoint=offset_viewpoint,
                                                  offset_samples=offset_samples,
                                                  BPPARAM=bpparam)

    visible_coordinates <- as.matrix(visible_coordinates) #convert matrix to dataframe
    visible_coordinates <- visible_coordinates[visible_coordinates[,1]!="NULL"]
    #convert list to matrix
    visible_coordinates <- matrix(unlist(visible_coordinates),
                                  ncol = length(visible_coordinates),
                                  nrow = 3)
    visible_coordinates <- base::t(visible_coordinates)
    #convert matrix to dataframe
    visible_coordinates <- as.data.frame(visible_coordinates)
    # remove the column of elevation for conversion of spatial point
    visible_coordinates <- visible_coordinates[-c(3)]
    if(length(visible_coordinates[,1]) < 1){
      stop("There is no visible point detected")
    }

    print("Completed!")
    end_time <- Sys.time()
    time_taken <- end_time - start_time
    print(time_taken)
    return(visible_coordinates)
  }else if (multiviewpoints == TRUE){
    # set a new empty vector
    viewscape_v <- c()

    for(i in 1:length(viewpoints)){
      viewpoint <- c(viewpoints[i,1],viewpoints[i,2])
      start_time <- Sys.time()
      if(is.null(r) == TRUE){
        if(is.null(sample_points) == TRUE){
          #convert raster to matrix
          sample_points <- raster::rasterToPoints(dsm)
        }
      }else{
        # create an extent to crop input raster
        pdf <- data.frame(row.names = 1)
        pdf[1,"x"] <- viewpoint[1]
        pdf[1,"y"] <- viewpoint[2]
        p <- sp::SpatialPoints(pdf)
        p <- sf::st_as_sf(p)
        subarea <- sf::st_buffer(p, r)
        subdsm <- raster::crop(dsm, raster::extent(subarea))
        dsm <- subdsm
        if(is.null(under) == FALSE){
          under <- raster::crop(under, raster::extent(subarea))
        }
        if(is.null(sample_points) == TRUE|is.null(sample_points) == FALSE){
          #convert raster to matrix
          sample_points <- raster::rasterToPoints(subdsm)
        }
      }

      bpparam <- BiocParallel::SnowParam(workers=detectCores(), type="FORK")
      visible_coordinates <- BiocParallel::bplapply(X = split(sample_points,seq(nrow(sample_points))),
                                                    FUN = visiblesample,
                                                    dsm = dsm, modified_dsm=under,
                                                    dem=dem, viewpoint = viewpoint,
                                                    BPPARAM=bpparam)

      visible_coordinates <- as.matrix(visible_coordinates)
      visible_coordinates <- visible_coordinates[visible_coordinates[,1]!="NULL"]
      #convert list to matrix
      visible_coordinates <- matrix(unlist(visible_coordinates),
                                    ncol = length(visible_coordinates),
                                    nrow = 3)
      visible_coordinates <- base::t(visible_coordinates)
      #convert matrix to dataframe
      visible_coordinates <- as.data.frame(visible_coordinates)
      # remove the column of elevation for conversion of spatial point
      visible_coordinates <- visible_coordinates[-c(3)]
      if(length(visible_coordinates[,1]) < 1){
        print("There is no visible point detected from this viewpoint")
      }else{
        viewscape_v <- c(viewscape_v, visible_coordinates)
      }
      return(viewscape_v)
    }
  }
}
