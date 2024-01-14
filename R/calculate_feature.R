#' calculate_feature
#' @description The calculate_feature function is designed to extract specific
#' feature-related information within a viewshed. It allows you to compute
#' the proportion of the feature that is present in the viewshed.
#'
#' @param type Numeric. The input type of land cover raster.
#' type=1: percentage raster (that represents the percentage of
#' area in each cell).
#' type=2: binary raster (that only uses two values to represent whether
#' the feature exists in each cell).
#' @param feature Raster. Land cover or land use
#' @param viewshed Viewshed object.
#' @param exclude_value Numeric. the value of those cells need to be excluded
#' in the analysis. If type = 2, exclude_value is reqired.
#'
#' @return Numeric. The canopy area in the viewshed.
#' @export
#'
#' @examples
#' \dontrun{
#' # load canopy raster
#' # test_canopy <- terra::rast(system.file("test_canopy.tif",
#' #                                      package ="viewscape"))
#' # calculate the percentage of canopy coverage
#' # test_canopy_proportion <- viewscape::calculate_feature(type = 2,
#' #                                                       feature = test_canopy,
#' #                                                       viewshed = viewshed
#' #                                                       exclude_value = 0)
#' }

calculate_feature <- function(type,
                              feature,
                              viewshed,
                              exclude_value){
  if (isFALSE(terra::crs(feature, proj = TRUE) == viewshed@crs)) {
    cat("Your input (feature) rasters have different
        coordinate reference system from the viewshed\n")
    cat("Reprojetion will be processing ...\n")
    feature <- terra::project(feature, y=terra::crs(viewshed@crs))
  }
  if (missing(type)) {
    stop("type is missing")
  }
  if (type == 2 & missing(exclude_value)) {
    stop("please specify exclude_value")
  }
  pt <- filter_invisible(viewshed, FALSE)
  feature_df <- terra::extract(feature, pt)[,1]
  # colnames(feature_df)[2] <- 'value'
  # feature_df <- subset(feature_df, value != exclude_value)
  if(type == 1){
    output <- sum((viewshed@resolution[1])^2*feature_)/
      (viewshed@resolution[1])^2*length(pt[,1])
  }
  else if(type == 2){
    feature_ <- feature_df[feature_df!=exclude_value]
    output <- length(feature_)/length(pt[,1])
  }
  return(output)
}
