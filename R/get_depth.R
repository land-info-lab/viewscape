#' get_depth
#'
#' @param visiblepoints The viewshed calulated by function 'calculate_viewshed'
#' @param viewpoint A matrix including x,y coordinates
#' @param type numeric, indicating the type of metric to be returned. 1 = depth (Furthest distance);
#' 2 = depth variation; 3 = both (a list).
#'
#' @return Numeric. If type = 1 or 2, the function will return furthest distance
#' or standard deviation of distance as a number. If type = 3, the function will
#' return both metrics above as a vector.
#' @export
#'
#' @examples
get_depth <- function(visiblepoints, viewpoint, type){
  ##visiblepoints is the viewshed calulated by function 'calculate_viewshed'
  ##viewpoint is a matrix including x,y coordinates
  ##type is the metric to be returned. 1=depth(Furthest distance);
  #2=depth variation; 3=both (a list)
  depths <- c()
  for (i in 1:nrow(visiblepoints)) {
    distance <- sqrt( (viewpoint[1]-visiblepoints$x[i])^2 +
                        (viewpoint[2]-visiblepoints$y[i])^2 )
    depths <- c(depths, distance)
  }
  furthest_distance <- max(depths)
  depth_variation <- sd(depths)
  if(type == 1){
    return(furthest_distance)
  }
  else if(type == 2){
    return(depth_variation)
  }
  else if(type == 3){
    return(c(furthest_distance, depth_variation))
  }
}
