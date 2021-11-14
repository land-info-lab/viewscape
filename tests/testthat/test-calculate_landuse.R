test_that("runs correctly", {

  #load files
  test_dsm <- raster::raster(system.file("test_data/test_dsm.tif",
                                         package ="viewscape"))
  test_landuse <- raster::raster(system.file("test_data/test_landuse.tif",
                                               package ="viewscape"))

  test_viewpoint <- sf::read_sf(system.file("test_data/test_viewpoint.shp",
                                            package = "viewscape"))

  test_viewpoint <- sf::st_coordinates(test_viewpoint)

  test_viewpoint <- c(test_viewpoint[,1], test_viewpoint[,2])

  # get visible points based on viewpoint
  test_visiblepoint <- viewscape::calculate_viewshed(dsm = test_dsm,
                                                     viewpoint = test_viewpoint)

  #run function
  test_landuse_area <- viewscape::calculate_landuse(landuse = test_landuse, dsm = test_dsm,
                                                    visiblepoints = test_visiblepoint)

  expect_type(test_landuse_area, "double")
})
