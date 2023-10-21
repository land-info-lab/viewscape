# Viewscape

This vignette provides a basic overview of the functions in R package
`viewscape`.

The basic viewshed analysis can be accessed through calling the
`compute_viewshed`. The two needed objects to compute the viewshed are a
digital surface model (DSM) and a viewpoint.

## 1. Compute viewshed

    library(viewscape)

    ## Warning: replacing previous import 'imager::bbox' by 'lidR::bbox' when loading
    ## 'viewscape'

    ## Warning: replacing previous import 'imager::watershed' by 'lidR::watershed' when
    ## loading 'viewscape'

    ## Warning: replacing previous import 'lidR::projection<-' by
    ## 'raster::projection<-' when loading 'viewscape'

    ## Warning: replacing previous import 'lidR::projection' by 'raster::projection'
    ## when loading 'viewscape'

    ## loading... 
    ## MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
    ## MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
    ## MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM/MMM/MMM/M///MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
    ## MMMMMMMMMMMMMMMMMMMMMMMMMMMMMM/M////MM/M/MM//////MM//MM///MM///MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
    ## MMMMMMMMMMMMMMMMMMMMMM/M////MM//MM/MM/MMMM//MMM///MM///MM/////MM//MM/MMMMMMMMMMMMMMMMMMMMMMMMMMMMM
    ## MMMMMMMMMMMMMMMMM/////MM/MM/M/MMM/mn=======nmMMMM/M/MM/////MMM///MMM/MMMMMMMMMMMMMMMMMMMMMMMMMMMMM
    ## MMMMMMMMMMMM////MM/MMMMMMmn-=====nMMMMMMMMMMn=====nmMMMMMM///MMM//MMM////M/MMMMMMMMMMMMMMMMMMMMMMM
    ## MMMMMMMMM///M/MMMMmm-======nMMMMm:::::::::::NMMMn-=======nmMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
    ## MMMMMM//MMMMMMm-====nMMMN::::::..............::::::MMMMn-======nmMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
    ## MMM///MMMM-===nMMN:::::::....////XXXooo...........::::::::NMMn-====nmMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
    ## MMMMMMM-==MMn:::...........////XX??-||-oo................:::::::::NMM-===MMMMMMMMMMMMMMMMMMMMMMMMM
    ## MMM-===MNn:..............///XXXX??------oo.......................::::MMM==MMMMMMMMMMMMMMMMMMMMMMMM
    ## MMMM==:.................iiii'''ii??----oooo...................::::MMM-===MMMMMMMMMMMMMMMMMMMMMMMMM
    ## MMMMM==:...............iii'''''iiiii////oooo................:::MMMM-===MMMMMMMMMMMMMMMMMMMMMMMMMMM
    ## MMMMMMM==:::............iii''''''iii///oooo..............:::::MMM-===MMMMMMMMMMMMMMMMMMMMMMMMMMMMM
    ## MMMMMMMMM==::::.........iii'''''ooo//ooooo.........::::::MMMM-==MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
    ## MMMMMMMMMMMM==::::::.....iioo'''''//ooooo.....::::::MMMM-==MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
    ## MMMMMMMMMMMMMMMM==:::::::..ioooooooooo:::::::::MMMMMM==MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
    ## MMMMMMMMMMMMMMMMMMMMMMMMMMMMMmmmmmmmmmmMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
    ## MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
    ## MMM  MMMMMMMMM  MM  MM      MM  MMMM  MMMM  MM.......MMM.......MMMMM...MMMMM.........MMMM......MMM
    ## MMMM  MMMMMMM  MM  MM  MMMMMM  MMM    MMM  MM..MMMMMMMM..MMMMM..MMM..M..MMMM..MMMMMM..MM..MMMMMMMM
    ## MMMMM  MMMMM  MM  MM  MMMMMM  MM  MM  MM  MM........MM..MMMMMMMMMM..MM..MMM..MMMMMM..MM..MMMMMMMMM
    ## MMMMMM  MMM  MM  MM      MM  M  MMMM  M  MMMMMMMMM..MM..MMMMMMMMM.......MMM.........MM.......MMMMM
    ## MMMMMMM  M  MM  MM  MMMMMM   MMMMMMM   MMM..MMMM...MM...MMMM..MM..MMMM..MMM..MMMMMMMM..MMMMMMMMMMM
    ## MMMMMMMM   MM  MM      MM  MMMMMMMMM  MMM.........MMM........MM..MMMMM..MM..MMMMMMMM.......MMMMMMM
    ## MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
    ## MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM

### 1.1 Compute single viewshed

    #Load in DSM
    test_dsm <- raster::raster(system.file("test_dsm.tif", 
                                           package ="viewscape"))

    #Load in the viewpoint
    test_viewpoint <- sf::read_sf(system.file("test_viewpoint.shp", 
                                              package = "viewscape"))

    #Transform viewpoint from shape file to coordinates 
    test_viewpoint <- sf::st_coordinates(test_viewpoint)
    test_viewpoint <- c(test_viewpoint[,1], test_viewpoint[,2])

    #Compute viewshed
    output <- viewscape::compute_viewshed(dsm = test_dsm, 
                                          viewpoints = test_viewpoint, 
                                          offset_viewpoint = 6, 
                                          plot=TRUE)

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
    test_dsm <- raster::raster(system.file("test_dsm.tif", 
                                           package ="viewscape"))

    # Load points (.shp file)
    test_viewpoints <- sf::read_sf(system.file("test_viewpoints.shp", 
                                               package = "viewscape"))

    #Transform viewpoint from shape file to coordinates 
    test_viewpoints <- sf::st_coordinates(test_viewpoints)

    # Compute viewsheds
    # the output includes binary matrix and extent of viewshed 
    output <- viewscape::compute_viewshed(dsm = test_dsm, 
                                          viewpoints = test_viewpoints, 
                                          offset_viewpoint = 6, 
                                          multiviewpoints = TRUE,
                                          raster=TRUE)

    # Use plot all viewsheds on DSM
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

The function of view depth analysis can calculate two different metrics:
the furthest distance and standard deviation of distances. To calculate
view depth, there are two needed objects: the DSM that was used to get
viewshed and result from viewshed analysis.

The function of extent analysis can calculate the total area of viewshed
and needs the DSM that was used to get viewshed and result from viewshed
analysis.

The following function can calculate the area of ground surface and
standard deviation of elevations within a viewshed. The function needs a
DSM and a DEM/DTM to calculate the metrics.

    # Load DTM
    test_dtm <- raster::raster(system.file("test_dtm.tif", 
                                           package ="viewscape"))

    # Load canopy raster
    test_canopy <- raster::raster(system.file("test_canopy.tif", 
                                           package ="viewscape"))

    # Load building footprints raster
    test_building <- raster::raster(system.file("test_building.tif", 
                                           package ="viewscape"))

    raster::plot(test_dtm)

![](/private/var/folders/8t/yvsjl1wd01z9y49tb6zbyszw0000gn/T/RtmpCIg1Uj/preview-ed46e4fdb6d.dir/viewscape_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    raster::plot(test_canopy)

![](/private/var/folders/8t/yvsjl1wd01z9y49tb6zbyszw0000gn/T/RtmpCIg1Uj/preview-ed46e4fdb6d.dir/viewscape_files/figure-markdown_strict/unnamed-chunk-7-1.png)

    raster::plot(test_building)

![](/private/var/folders/8t/yvsjl1wd01z9y49tb6zbyszw0000gn/T/RtmpCIg1Uj/preview-ed46e4fdb6d.dir/viewscape_files/figure-markdown_strict/unnamed-chunk-8-1.png)

    # calculate metrics given the viewshed
    test_metrics <- viewscape::calculate_viewmetrics(output[[1]], 
                                                     test_dsm, 
                                                     test_dtm, 
                                                     list(test_canopy, test_building))
    test_metrics

    ## $extent
    ## [1] 61347.37
    ## 
    ## $depth
    ## [1] 1018.219
    ## 
    ## $vdepth
    ## [1] 237.4841
    ## 
    ## $horizontal
    ## [1] 42701.88
    ## 
    ## $relief
    ## [1] 0.675813
    ## 
    ## $skyline
    ## [1] 3.760434

### 2.2 Calculate land use/cover diversity

calculate\_diversity() calculates the proportion of each type of land
use/ cover within a viewshed to get the Shannon Diversity Index.

    # load landuse raster
    test_landcover <- raster::raster(system.file("test_landuse.tif",
                                               package ="viewscape"))
    raster::plot(test_landcover, axes=FALSE)

![](/private/var/folders/8t/yvsjl1wd01z9y49tb6zbyszw0000gn/T/RtmpCIg1Uj/preview-ed46e4fdb6d.dir/viewscape_files/figure-markdown_strict/unnamed-chunk-10-1.png)

    i# the Shannon Diversity Index (SDI)

    ## [1] 8

    test_diversity <- viewscape::calculate_diversity(test_landcover, 
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
    test_canopy <- raster::raster(system.file("test_canopy.tif",
                                              package ="viewscape"))
    # calculate the percentage of each type of land use  
    test_canopy_proportion <- viewscape::calculate_feature(type = 2, 
                                                           feature = test_canopy,
                                                           viewshed = output[[1]])
    test_canopy_proportion

## 3. Get LiDAR data

### 3.1 Get LiDAR data information via API

    # search for lidar data information using bbox
    search_result <- viewscape::lidar_search(bbox = c(-83.742282,42.273389,-83.733442,42.278724), preview = TRUE)

    ## Find 5 items

    search_result

    ##                                               titles                 sourceId
    ## 1 USGS Lidar Point Cloud MI_31County_2016_A16 290280 64bb1858d34e70357a2dcbbf
    ## 2 USGS Lidar Point Cloud MI_31County_2016_A16 290282 64bb1624d34e70357a2dc1c7
    ## 3 USGS Lidar Point Cloud MI_31County_2016_A16 292280 64bb16bbd34e70357a2dc429
    ## 4 USGS Lidar Point Cloud MI_31County_2016_A16 292282 64bb1f09d34e70357a2dd90a
    ## 5  USGS Lidar Point Cloud MI_WASHTENAWCO_2009 000580 641910f0d34eb496d1d2736d
    ##                                                                         metaUrl
    ## 1 https://www.sciencebase.gov/catalog/item/64bb1858d34e70357a2dcbbf?format=json
    ## 2 https://www.sciencebase.gov/catalog/item/64bb1624d34e70357a2dc1c7?format=json
    ## 3 https://www.sciencebase.gov/catalog/item/64bb16bbd34e70357a2dc429?format=json
    ## 4 https://www.sciencebase.gov/catalog/item/64bb1f09d34e70357a2dd90a?format=json
    ## 5 https://www.sciencebase.gov/catalog/item/641910f0d34eb496d1d2736d?format=json
    ##   sizeInBytes startYear
    ## 1     8461540      2017
    ## 2    10843574      2017
    ## 3    10101484      2017
    ## 4    11134390      2017
    ## 5    51400999      2009
    ##                                                                                                                                                    previewGraphicURL
    ## 1 https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/LPC/Projects/MI_31County_2016_A16/MI_31Co_Washtenaw_2016/browse/USGS_LPC_MI_31County_2016_A16_290280.jpg
    ## 2 https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/LPC/Projects/MI_31County_2016_A16/MI_31Co_Washtenaw_2016/browse/USGS_LPC_MI_31County_2016_A16_290282.jpg
    ## 3 https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/LPC/Projects/MI_31County_2016_A16/MI_31Co_Washtenaw_2016/browse/USGS_LPC_MI_31County_2016_A16_292280.jpg
    ## 4 https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/LPC/Projects/MI_31County_2016_A16/MI_31Co_Washtenaw_2016/browse/USGS_LPC_MI_31County_2016_A16_292282.jpg
    ## 5                   https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/LPC/Projects/legacy/MI_WASHTENAWCO_2009/browse/USGS_LPC_MI_WASHTENAWCO_2009_000580.jpg
    ##                                                                                                                                                        downloadLazURL
    ## 1 https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/LPC/Projects/MI_31County_2016_A16/MI_31Co_Washtenaw_2016/LAZ/USGS_LPC_MI_31County_2016_A16_290280.laz
    ## 2 https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/LPC/Projects/MI_31County_2016_A16/MI_31Co_Washtenaw_2016/LAZ/USGS_LPC_MI_31County_2016_A16_290282.laz
    ## 3 https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/LPC/Projects/MI_31County_2016_A16/MI_31Co_Washtenaw_2016/LAZ/USGS_LPC_MI_31County_2016_A16_292280.laz
    ## 4 https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/LPC/Projects/MI_31County_2016_A16/MI_31Co_Washtenaw_2016/LAZ/USGS_LPC_MI_31County_2016_A16_292282.laz
    ## 5                   https://rockyweb.usgs.gov/vdelivery/Datasets/Staged/Elevation/LPC/Projects/legacy/MI_WASHTENAWCO_2009/LAZ/USGS_LPC_MI_WASHTENAWCO_2009_000580.laz

![](/private/var/folders/8t/yvsjl1wd01z9y49tb6zbyszw0000gn/T/RtmpCIg1Uj/preview-ed46e4fdb6d.dir/viewscape_files/figure-markdown_strict/unnamed-chunk-13-1.png)

### 3.2 Download LiDAR data with a given point and searching distance

    # try coordinates -83.741289,42.270146 (in south Michigan, USA)
    # radius is 1000ft
    las <- viewscape::get_lidar(x = -83.741289,
                                y = 42.270146,
                                r = 1000,
                                epsg = 2253,
                                folder = '/Users/yangxiaohao/Downloads/testfunction',
                                plot = FALSE)

    ## Warning: PROJ support is provided by the sf and terra packages among others

    ## Find 3 items
    ## Downloading 2 file(s)...

    # Create DTM
    dtm_ <- lidR::rasterize_terrain(las, res = 5, lidR::tin())

    ## Warning: Interpolation of 371 points failed because they are too far from ground
    ## points. Nearest neighbour was used but interpolation is weak for those points

    raster::plot(dtm_)

![](/private/var/folders/8t/yvsjl1wd01z9y49tb6zbyszw0000gn/T/RtmpCIg1Uj/preview-ed46e4fdb6d.dir/viewscape_files/figure-markdown_strict/unnamed-chunk-15-1.png)

    # Create DSM
    dsm_ <- lidR::rasterize_canopy(las, res = 5, lidR::dsmtin())
    raster::plot(dsm_)

![](/private/var/folders/8t/yvsjl1wd01z9y49tb6zbyszw0000gn/T/RtmpCIg1Uj/preview-ed46e4fdb6d.dir/viewscape_files/figure-markdown_strict/unnamed-chunk-16-1.png)

For more usages of lidR please refer:
<https://github.com/r-lidar/lidR/tree/master> and
<https://rpubs.com/jesseast/lidR4smarties>
