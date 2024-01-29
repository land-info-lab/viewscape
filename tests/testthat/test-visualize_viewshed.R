testthat::test_that("runs correctly", {

  #Load in DSM
  test_dsm <- terra::rast(system.file("test_dsm.tif",
                                      package ="viewscape"))

  #Load in the viewpoint
  test_viewpoint <- sf::read_sf(system.file("test_viewpoint.shp",
                                            package = "viewscape"))

  #Compute viewshed
  output <- viewscape::compute_viewshed(dsm = test_dsm,
                                        viewpoints = test_viewpoint,
                                        offset_viewpoint = 6,
                                        plot=FALSE)

  # the viewshed raster output
  output_r <- viewscape::visualize_viewshed(output, outputtype = 'raster')

  testthat::expect_type(output_r, "S4")
})
