
<!-- badges: start -->

[![R-CMD-check](https://github.com/billbillbilly/viewscape/workflows/R-CMD-check/badge.svg)](https://github.com/billbillbilly/viewscape/actions)
[![Codecov test
coverage](https://codecov.io/gh/billbillbilly/viewscape/branch/master/graph/badge.svg)](https://codecov.io/gh/billbillbilly/viewscape?branch=master)
<!-- badges: end -->

# viewscape

<p align="left">

<img src=".//man//figures//viewscape_hex.png" height="200">

</p>

## Introduction

The goal of viewscape package is to provide an accessible method of
carrying out viewshed analysis within the R environment. The viewscape R
pacakge can currently be downloaded via github.

``` r
library(devtools)

install_github("billbillbilly/viewscape")
```

The basic viewshed analysis can be accessed through calling the
`calculate_viewshed`. The two needed objects to calculate the viewshed
are a digital surface model (DSM) and a viewpoint.

``` r
  #Load in DSM
  test_dsm <- raster::raster(system.file("test_data\\test_dsm.tif",
                                         package ="viewscape"))

  #Load in the viewpoint
  test_viewpoint <- sf::read_sf(system.file("test_data\\test_viewpoint.shp",
                                               package = "viewscape"))

  #Transform viewpoint from shape file to coordinates 
  test_viewpoint <- sf::st_coordinates(test_viewpoint)
  test_viewpoint <- c(test_viewpoint[,1], test_viewpoint[,2])

  #Run function
  test_function <- viewscape::calculate_viewshed(dsm = test_dsm,
                                                 viewpoint = test_viewpoint)
```

For further information on the rest of the functions available in this
package please refer to the [package
website](needs%20to%20be%20created). For more information and examples
of the functions check out the [package
vignette](needs%20to%20be%20created).

## Issues and bugs

This package may take a long time to run if using spatially large or
high resolution digital elevation models.

If you discover a bug not associated with connection to the API that is
not already a [reported
issue](https://github.com/billbillbilly/viewscape/issues), please [open
a new issue](https://github.com/billbillbilly/viewscape/issues/new)
providing a reproducible example.
