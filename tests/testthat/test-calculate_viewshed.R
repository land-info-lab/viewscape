context("Calculate viewshed")

test_that("runs correctly", {

  #load files
  test_dsm <- raster::raster(system.file("test_data\\test_dsm.tif",
                                         package ="viewscape"))

  test_viewpoint <- sf::read_sf(system.file("test_data\\test_viewpoint.shp",
                                               package = "viewscape"))

  test_viewpoint <- sf::st_coordinates(test_viewpoint)

  test_viewpoint <- c(test_viewpoint[,1], test_viewpoint[,2])

  #run function
  test_function <- viewscape::calculate_viewshed(dsm = dsm,
                                                 viewpoint = test_viewpoint)


})
