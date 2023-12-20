testthat::test_that("runs correctly", {

  #Load in DSM
  test_dsm <- terra::rast(system.file("test_dsm.tif",
                                      package ="viewscape"))

  # load canopy raster
  test_canopy <- terra::rast(system.file("test_canopy.tif",
                                         package ="viewscape"))

  #Load in the viewpoint
  test_viewpoint <- sf::read_sf(system.file("test_viewpoint.shp",
                                            package = "viewscape"))

  #Compute viewshed
  output <- viewscape::compute_viewshed(dsm = test_dsm,
                                        viewpoints = test_viewpoint,
                                        offset_viewpoint = 6,
                                        plot=FALSE)

  # calculate the percentage of canopy coverage
  test_canopy_proportion <- viewscape::calculate_feature(type = 2,
                                                         feature = test_canopy,
                                                         viewshed = output)

  testthat::expect_type(test_canopy_proportion, "double")

})
