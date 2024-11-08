testthat::test_that("runs correctly", {

  #Load in DSM
  test_dsm <- terra::rast(system.file("test_dsm.tif",
                                      package ="viewscape"))

  # Load DTM
  test_dtm <- terra::rast(system.file("test_dtm.tif",
                                      package ="viewscape"))

  # Load canopy raster
  test_canopy <- terra::rast(system.file("test_canopy.tif",
                                         package ="viewscape"))

  # Load building footprints raster
  test_building <- terra::rast(system.file("test_building.tif",
                                           package ="viewscape"))

  #Load in the viewpoint
  test_viewpoint <- sf::read_sf(system.file("test_viewpoint.shp",
                                            package = "viewscape"))

  #Compute viewshed
  output <- viewscape::compute_viewshed(dsm = test_dsm,
                                        viewpoints = test_viewpoint,
                                        offset_viewpoint = 6)

  # calculate metrics given the viewshed
  test_metrics <- viewscape::calculate_viewmetrics(output,
                                                   test_dsm,
                                                   test_dtm,
                                                   list(test_canopy, test_building))
  testthat::expect_type(test_metrics, "list")

})
