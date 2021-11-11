#' calculate_viewshed
#'
#' @param dsm the raster layer of digital surface model/digital elevation model
#' @param under the raster layer of digital surface model where the elevation
#' above crown base of trees was removed
#' @param dem the raster of terrain without any vertical object. If input of
#' dsm is already digital elevation model, this argument can be ignored
#' @param viewpoint a matrix including x,y coordinates
#' @param sample_points a matrix of points that are converted from DSM.
#' This can be ignored. (it is defaulted as NULL)
#' @param r the radius for viewshed analysis. (it is defaulted as NULL)
#'
#' @return Dataframe. The output dataframe includes x anf y coordinates.
#' @export
#'
#' @examples
#'
calculate_viewshed <- function(dsm,
                               under=NULL,
                               dem=NULL,
                               viewpoint,
                               sample_points=NULL,
                               r = NULL){

  # calculate viewshed based on a certain viewpoint

  ## dsm is raster layer of digital surface model/digital elevation model
  ## under is raster layer of digital surface model where the elevation above
  #crown base of trees was removed

  ## dem is the raster of terrain without any vertical object. If input of dsm
  #is already digital elevation model, this arguement can be ignored

  ## viewpoint is a matrix including x,y coordinates
  ## sample_points is a matrix of points that are converted from DSM. This can
  #be ignored. (it is defaulted as NULL)

  ## r is the radius for viewshed analysis. (it is defaulted as NULL)

  if(is.null(r) == TRUE){
    if(is.null(sample_points) == TRUE){
      sample_points <- raster::rasterToPoints(dsm) #convert raster to matrix
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
      sample_points <- raster::rasterToPoints(subdsm) #convert raster to matrix
    }

  }

  visiblepoints <- matrix(0,1,3) #set an empty matrix

  for(i in 1:nrow(sample_points)){
    #calculate the distance between viewpoint and each cell(point) of(on) the
    #raster(DSM) and extract visible points.
    #read coordinates of each position (cell in raster) one by one
    samplecoordinates <- sample_points[i,]

    visibles <- visiblesample(dsm,
                              modified_dsm = under,
                              dem = dem,
                              viewpoint,
                              samplecoordinates) #check visibility

    if(is.null(visibles) == FALSE){# if the position of sample can be seen
      #add this position into the matrix
      visiblepoints <- rbind(visiblepoints, visibles)
    }
  }

  if(length(visiblepoints[,1]) == 1){
    stop("There is no visible point detected")
  }

  #remove the initial row, which is matrix(0, 1,3)
  visiblepoints <- visiblepoints[2:nrow(visiblepoints),]
  visiblepoints <- as.data.frame(visiblepoints) #convert matrix to dataframe

  # remove the column of elevation for conversion of spatial point
  visible_coordinates <- visiblepoints[-c(3)]
  return(visible_coordinates)

}
