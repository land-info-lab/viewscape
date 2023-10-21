
<!-- badges: start -->

[![R-CMD-check](https://github.com/billbillbilly/viewscape/workflows/R-CMD-check/badge.svg)](https://github.com/billbillbilly/viewscape/actions)
[![Codecov test
coverage](https://codecov.io/github/billbillbilly/viewscape/branch/master/graph/badge.svg)](https://codecov.io/github/billbillbilly/viewscape?branch=master)
<!-- badges: end -->

# viewscape

<p align="left">

<img src=".//man//figures//viewscape_hex.png" height="200">

</p>

## Introduction

The goal of viewscape package is to provide an accessible method of
carrying out landscape spatial analysis based on the viewshed within the
R environment. The viewscape R pacakge can currently be downloaded via
github.

``` r
library(devtools)

install_github("land-info-lab/viewscape")
```

The basic viewshed analysis can be accessed through calling the
`compute_viewshed`. The two needed objects are a digital surface model
(DSM) and a viewpoint. It provides flexibility for single or
multi-viewpoint analyses and allows options for parallel processing,
raster output, and plotting.

Based on the viewshed, a set of configuration metrics can be calculated,
including: 1. Extent: The total area of the viewshed, calculated as the
number of visible grid cells multiplied by the grid resolution.. Depth:
The furthest visible distance within the viewshed from the viewpoint..
Vdepth: The standard deviation of distances to visible points, providing
a measure of the variation in visible distances.. Horizontal: The total
visible horizontal or terrestrial area within the viewshed.. Relief: The
standard deviation of elevations of the visible ground surface..
Skyline: Variation of (Standard deviation) of the vertical viewscape
(visible canopy and buildings).. Shannon diversity index: Based on the
number of land use/cover classes and the proportion of distribution..
Proportion of other object: Building, trees, or paved surface.

``` r
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

# Load DTM
test_dtm <- raster::raster(system.file("test_dtm.tif", 
                                       package ="viewscape"))

# load landuse raster
test_landcover <- raster::raster(system.file("test_landuse.tif",
                                           package ="viewscape"))

# Load canopy raster
test_canopy <- raster::raster(system.file("test_canopy.tif", 
                                       package ="viewscape"))

# Load building footprints raster
test_building <- raster::raster(system.file("test_building.tif", 
                                       package ="viewscape"))

# calculate metrics given the viewshed
test_metrics <- viewscape::calculate_viewmetrics(output, 
                                                 test_dsm, 
                                                 test_dtm, 
                                                 list(test_canopy, test_building))

# the Shannon Diversity Index (SDI)
test_diversity <- calculate_diversity(test_landcover, output, proportion = TRUE)
```

Baesd on the TNMAccess API, LiDAR search facilitate the retrieval and
exploration of LiDAR (Light Detection and Ranging) data (from USGS)
within a specified bounding box (bbox). This function enables users to
search for LiDAR data, preview available graphics, and optionally
download LiDAR data files for further viewscape analysis. Current
dataset of USGS covers the most area in the US:
<https://apps.nationalmap.gov/lidar-explorer/#/>

``` r
# search for lidar data information using bbox
search_result <- viewscape::lidar_search(bbox = c(-83.742282,42.273389,-83.733442,42.278724), preview = TRUE)
#> Warning: replacing previous import 'imager::bbox' by 'lidR::bbox' when loading
#> 'viewscape'
#> Warning: replacing previous import 'imager::watershed' by 'lidR::watershed' when
#> loading 'viewscape'
#> Warning: replacing previous import 'lidR::projection<-' by
#> 'raster::projection<-' when loading 'viewscape'
#> Warning: replacing previous import 'lidR::projection' by 'raster::projection'
#> when loading 'viewscape'
#> loading... 
#> MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#> MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#> MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM/MMM/MMM/M///MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#> MMMMMMMMMMMMMMMMMMMMMMMMMMMMMM/M////MM/M/MM//////MM//MM///MM///MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#> MMMMMMMMMMMMMMMMMMMMMM/M////MM//MM/MM/MMMM//MMM///MM///MM/////MM//MM/MMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#> MMMMMMMMMMMMMMMMM/////MM/MM/M/MMM/mn=======nmMMMM/M/MM/////MMM///MMM/MMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#> MMMMMMMMMMMM////MM/MMMMMMmn-=====nMMMMMMMMMMn=====nmMMMMMM///MMM//MMM////M/MMMMMMMMMMMMMMMMMMMMMMM
#> MMMMMMMMM///M/MMMMmm-======nMMMMm:::::::::::NMMMn-=======nmMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#> MMMMMM//MMMMMMm-====nMMMN::::::..............::::::MMMMn-======nmMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#> MMM///MMMM-===nMMN:::::::....////XXXooo...........::::::::NMMn-====nmMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#> MMMMMMM-==MMn:::...........////XX??-||-oo................:::::::::NMM-===MMMMMMMMMMMMMMMMMMMMMMMMM
#> MMM-===MNn:..............///XXXX??------oo.......................::::MMM==MMMMMMMMMMMMMMMMMMMMMMMM
#> MMMM==:.................iiii'''ii??----oooo...................::::MMM-===MMMMMMMMMMMMMMMMMMMMMMMMM
#> MMMMM==:...............iii'''''iiiii////oooo................:::MMMM-===MMMMMMMMMMMMMMMMMMMMMMMMMMM
#> MMMMMMM==:::............iii''''''iii///oooo..............:::::MMM-===MMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#> MMMMMMMMM==::::.........iii'''''ooo//ooooo.........::::::MMMM-==MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#> MMMMMMMMMMMM==::::::.....iioo'''''//ooooo.....::::::MMMM-==MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#> MMMMMMMMMMMMMMMM==:::::::..ioooooooooo:::::::::MMMMMM==MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#> MMMMMMMMMMMMMMMMMMMMMMMMMMMMMmmmmmmmmmmMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#> MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#> MMM  MMMMMMMMM  MM  MM      MM  MMMM  MMMM  MM.......MMM.......MMMMM...MMMMM.........MMMM......MMM
#> MMMM  MMMMMMM  MM  MM  MMMMMM  MMM    MMM  MM..MMMMMMMM..MMMMM..MMM..M..MMMM..MMMMMM..MM..MMMMMMMM
#> MMMMM  MMMMM  MM  MM  MMMMMM  MM  MM  MM  MM........MM..MMMMMMMMMM..MM..MMM..MMMMMM..MM..MMMMMMMMM
#> MMMMMM  MMM  MM  MM      MM  M  MMMM  M  MMMMMMMMM..MM..MMMMMMMMM.......MMM.........MM.......MMMMM
#> MMMMMMM  M  MM  MM  MMMMMM   MMMMMMM   MMM..MMMM...MM...MMMM..MM..MMMM..MMM..MMMMMMMM..MMMMMMMMMMM
#> MMMMMMMM   MM  MM      MM  MMMMMMMMM  MMM.........MMM........MM..MMMMM..MM..MMMMMMMM.......MMMMMMM
#> MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#> MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
#> Find 5 items
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

From viewshed analysis, the visible area of a viewpoint is presented by
visible points. There are several viewshed metrics such as can be
calculated based on the visible points. For further information on these
metrics and the rest of the functions available in this package please
refer to the [package
website](https://billbillbilly.github.io/viewscape/). For more
information and examples of the functions check out the [package
vignette](needs%20to%20be%20created).

## Issues and bugs

This package may take a long time to run if using spatially large or
high resolution digital elevation models.

If you discover a bug not associated with connection to the API that is
not already a [reported
issue](https://github.com/billbillbilly/viewscape/issues), please [open
a new issue](https://github.com/billbillbilly/viewscape/issues/new)
providing a reproducible example.
