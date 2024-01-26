# Viewscape

This vignette provides a basic overview of the functions in R package
`viewscape`.

The basic viewshed analysis can be accessed through calling the
`compute_viewshed`. The two needed objects to compute the viewshed are a
digital surface model (DSM) and a viewpoint.

## 1. Compute viewshed

    library(viewscape)

### 1.1 Compute single viewshed

    #Load in DSM
    test_dsm <- terra::rast(system.file("test_dsm.tif", 
                            package ="viewscape"))

    #Load in the viewpoint
    test_viewpoint <- sf::read_sf(system.file("test_viewpoint.shp", 
                                              package = "viewscape"))

    #Compute viewshed
    output <- viewscape::compute_viewshed(dsm = test_dsm, 
                                          viewpoints = test_viewpoint, 
                                          offset_viewpoint = 6)

![](/private/var/folders/8t/yvsjl1wd01z9y49tb6zbyszw0000gn/T/RtmpCIg1Uj/preview-ed46e4fdb6d.dir/viewscape_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    # overlap viewshed on DSM
    viewpoint <- matrix(0,1,3)
    viewpoint[1,1] <- test_viewpoint[1]
    viewpoint[1,2] <- test_viewpoint[2]
    output[output[] == 0 ] = NA
    raster::plot(test_dsm, axes=FALSE, box=FALSE, legend = FALSE)
    raster::plot(output, add=TRUE, col = "red", axes=FALSE, box=FALSE, legend = FALSE)
    raster::plot(sp::SpatialPoints(viewpoint), add = TRUE, col = "blue", axes=FALSE, box=FALSE, legend = FALSE)

![](/private/var/folders/8t/yvsjl1wd01z9y49tb6zbyszw0000gn/T/RtmpCIg1Uj/preview-ed46e4fdb6d.dir/viewscape_files/figure-markdown_strict/unnamed-chunk-3-1.png)

### 1.2 Compute the viewshed for multiple viewpoints

    #Load in DSM
    test_dsm <- terra::rast(system.file("test_dsm.tif", 
                            package ="viewscape"))

    # Load points (.shp file)
    test_viewpoints <- sf::read_sf(system.file("test_viewpoints.shp", 
                                               package = "viewscape"))

    # Compute viewsheds
    output <- viewscape::compute_viewshed(dsm = test_dsm, 
                                          viewpoints = test_viewpoints, 
                                          offset_viewpoint = 6, 
                                          parallel = TRUE, 
                                          workers = 8)

    # plot all viewsheds on DSM
    par(mfrow=c(3,3))
    for(i in 1:length(output)) {
      each <- output[[i]]
      raster_data <- viewscape::visualize_viewshed(each, outputtype="raster")
      raster::plot(test_dsm, axes=FALSE, box=FALSE, legend = FALSE)
      raster::plot(raster_data, add=TRUE, col = "red", axes=FALSE, box=FALSE, legend = FALSE)
    }

![](/private/var/folders/8t/yvsjl1wd01z9y49tb6zbyszw0000gn/T/RtmpCIg1Uj/preview-ed46e4fdb6d.dir/viewscape_files/figure-markdown_strict/unnamed-chunk-5-1.png)

## 2. Calculate viewscape metrics

### 2.1 Calculate the metrics of viewshed

The calculate_viewmetrics function is designed to compute a set of
configuration metrics based on a given viewshed object and optionally, digital surface
models (DSM) and digital terrain models (DTM) for terrain analysis.
The function calculates various metrics that describe the visibility characteristics
of a landscape from a specific viewpoint.

    # Load DTM
    test_dtm <- terra::rast(system.file("test_dtm.tif", 
                                        package ="viewscape"))

    # Load canopy raster
    test_canopy <- terra::rast(system.file("test_canopy.tif", 
                                           package ="viewscape"))

    # Load building footprints raster
    test_building <- terra::rast(system.file("test_building.tif", 
                                             package ="viewscape"))


![](/private/var/folders/8t/yvsjl1wd01z9y49tb6zbyszw0000gn/T/RtmpCIg1Uj/preview-ed46e4fdb6d.dir/viewscape_files/figure-markdown_strict/unnamed-chunk-8-1.png)

    # calculate metrics given the viewshed
    test_metrics <- viewscape::calculate_viewmetrics(output[[1]], 
                                                     test_dsm, 
                                                     test_dtm, 
                                                     list(test_canopy, test_building))
    test_metrics

### 2.2 Calculate land use/cover diversity

calculate\_diversity() calculates the proportion of each type of land
use/ cover within a viewshed to get the Shannon Diversity Index.

    # load landuse raster
    test_landuse <- terra::rast(system.file("test_landuse.tif",
                                            package ="viewscape"))

![](/private/var/folders/8t/yvsjl1wd01z9y49tb6zbyszw0000gn/T/RtmpCIg1Uj/preview-ed46e4fdb6d.dir/viewscape_files/figure-markdown_strict/unnamed-chunk-10-1.png)

    i# the Shannon Diversity Index (SDI)

    ## [1] 8

    test_diversity <- viewscape::calculate_diversity(test_landuse, 
                                                     output[[1]], 
                                                     proportion = TRUE)
    # SDI and The proportion of each type of land use
    test_diversity

    ## $SDI
    ## [1] 0.636
    ## 
    ## $Proportion
    ##                    1          2           3          4          5            6
    ## type       0.0000000 3.00000000 4.000000000 5.00000000 7.00000000 8.0000000000
    ## proportion 0.8315693 0.04011499 0.001502679 0.03913498 0.08715536 0.0005226708

### 2.3 calculate a single feature

calculate\_feature is to calculate the proportion of a feature
(including trees, buildings, parking, and roads) within the viewshed.
This function can be applied to

    # load landuse raster
    test_canopy <- terra::rast(system.file("test_canopy.tif",
                                           package ="viewscape"))
    # calculate the percentage of each type of land use  
    test_canopy_proportion <- viewscape::calculate_feature(type = 2, 
                                                           feature = test_canopy,
                                                           viewshed = output[[1]],
                                                           exclude_value=0)
    test_canopy_proportion
