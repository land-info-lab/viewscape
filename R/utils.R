samplealongline <- function(dsm, modified_dsm=NULL, dem=NULL, viewpoint, samplecoordinates){
  #this fuction can sample elevations along the straight line from viewpoint to a certain coordinates on raster surface
  #and also sample elevations on this line corresponding to the locations of sampled elevations on raster

  # dsm is raster layer of digital surface model/digital elevation model
  # modified_dsm is another dsm, where the elevation above crown base of trees was removed
  # dem is the raster of terrain without any vertical object. If input of dsm is already digital elevation model, this arguement can be ignored
  # viewpoint is a matrix including x,y coordinates
  # samplecoordinates is the coordinates of a certain cell(point) of a DSM
  viewline = sqrt( (viewpoint[1]-samplecoordinates[1])^2 + (viewpoint[2]-samplecoordinates[2])^2 ) #distance from viewpoint to sample point
  #sample a raster along a straight line between two points
  steps = 1 + round(viewline/ min(raster::res(dsm))) #try to match the sampling size to the raster resolution
  xc = viewpoint[1] + (0:steps) * (samplecoordinates[1]-viewpoint[1])/steps #x coordinates of samples along the viewline
  yc = viewpoint[2] + (0:steps) * (samplecoordinates[2]-viewpoint[2])/steps #y coordinates of samples along the viewline
  z_viewpoint = dsm[rts::cellFromXY(dsm,cbind(viewpoint[1],viewpoint[2]))] + 1.7*3 #the height of viewpoints based on dsm
  z_viewpoint2 = dem[rts::cellFromXY(dem,cbind(viewpoint[1],viewpoint[2]))] + 1.7*3 #the height of viewpoints based on dem
  if(is.null(modified_dsm) == FALSE){# if input modified_dsm is detected, compare the height of viewpoint on dsm to the one on dem
    if(is.null(dem) == TRUE){# request dsm
      stop("dem is NULL")
    }
    z_viewpoint_delta <- z_viewpoint - z_viewpoint2
    if(z_viewpoint_delta >= 3){# if the height of viewpoint on dsm is higher than the one on dem, modified_dsm will be used for viewshed analysis
      dsm <- modified_dsm
    }
  }
  z_sample = dsm[rts::cellFromXY(dsm,cbind(samplecoordinates[1],samplecoordinates[2]))] #set elevation of sample
  zc = dsm[rts::cellFromXY(dsm,cbind(xc,yc))] #extract elevation from dsm to the samples along the line based on corresponding coordinates
  # get elevation along the straight line at the same position of each step point
  if(z_viewpoint<z_sample){#when elevation of viewpoint is lower than sample
    zl <-  z_viewpoint + (0:steps)/steps*abs(z_viewpoint-z_sample)
  }else if(z_viewpoint>z_sample){#when elevation of viewpoint is higher than sample
    zl <-  z_viewpoint - (0:steps)/steps*abs(z_viewpoint-z_sample)
  }else if(z_viewpoint==z_sample){#when elevation of viewpoint equals to sample
    zl <- z_viewpoint + 0*(0:steps)
  }
  data.frame(x=xc, y=yc, z=zc, zl=zl)
}

visiblesample <- function(dsm, modified_dsm, dem, viewpoint, samplecoordinates){
  #this function determines if the positions of sampled elevations can be seen from the viewpoint

  # dsm is raster layer of digital surface model/digital elevation model
  # modified_dsm is another dsm, where the elevation above crown base of trees was removed
  # dem is the raster of terrain without any vertical object. If input of dsm is already digital elevation model, this arguement can be ignored
  # viewpoint is a matrix including x,y coordinates
  # samplecoordinates is the coordinates of a certain cell(point) of a DSM
  xyzzl <- samplealongline(dsm, modified_dsm = modified_dsm, dem = dem, viewpoint, samplecoordinates)
  xyzzl$z_delta <- xyzzl$zl - xyzzl$z
  xyzzl <- base::subset(xyzzl, is.na(xyzzl$z_delta)==FALSE , select = x:z_delta)
  min_delta <- min(xyzzl[5])
  if(min_delta>=0){#if there isn't any higher point between viewpoint and the sample
    return(samplecoordinates)
  }else{#if there is a higher point between viewpoint and the sample
    samplecoordinates <- NULL
    return(samplecoordinates)
  }
}

calculate_viewshed <- function(dsm, under=NULL, dem=NULL, viewpoint, sample_points=NULL, r = NULL){
  # calculate viewshed based on a certain viewpoint

  # dsm is raster layer of digital surface model/digital elevation model
  # under is raster layer of digital surface model where the elevation above crown base of trees was removed
  # dem is the raster of terrain without any vertical object. If input of dsm is already digital elevation model, this arguement can be ignored
  # viewpoint is a matrix including x,y coordinates
  # sample_points is a matrix of points that are converted from DSM. This can be ignored. (it is defaulted as NULL)
  # r is the radius for viewshed analysis. (it is defaulted as NULL)
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
    #calculate the distance between viewpoint and each cell(point) of(on) the raster(DSM) and extract visible points.
    samplecoordinates <- sample_points[i,] #read coordinates of each position (cell in raster) one by one
    visibles <- visiblesample(dsm, modified_dsm = under, dem = dem, viewpoint, samplecoordinates) #check visibility
    if(is.null(visibles) == FALSE){# if the position of sample can be seen
      visiblepoints <- rbind(visiblepoints, visibles) #add this position into the matrix
    }
  }
  if(length(visiblepoints[,1]) == 1){
    stop("There is no visible point detected")
  }
  visiblepoints <- visiblepoints[2:nrow(visiblepoints),] #remove the initial row, which is matrix(0, 1,3)
  visiblepoints <- as.data.frame(visiblepoints) #convert matrix to dataframe
  visible_coordinates <- visiblepoints[-c(3)]# remove the column of elevation for conversion of spatial point
  return(visible_coordinates)
}
