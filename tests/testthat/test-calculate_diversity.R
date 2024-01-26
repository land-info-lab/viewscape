testthat::test_that("runs correctly", {

  #Load in DSM
  test_dsm <- terra::rast(system.file("test_dsm.tif",
                                      package ="viewscape"))

  # load landuse raster
  test_landcover <- terra::rast(system.file("test_landuse.tif",
                                            package ="viewscape"))

  #Load in the viewpoint
  test_viewpoint <- sf::read_sf(system.file("test_viewpoint.shp",
                                            package = "viewscape"))

  #Compute viewshed
  output <- viewscape::compute_viewshed(dsm = test_dsm,
                                        viewpoints = test_viewpoint,
                                        offset_viewpoint = 6,
                                        plot=FALSE)

  # the Shannon Diversity Index (SDI)
  test_diversity <- viewscape::calculate_diversity(output,
                                                   test_landcover,
                                                   proportion = TRUE)

  testthat::expect_type(test_diversity[[1]], "double")

})
