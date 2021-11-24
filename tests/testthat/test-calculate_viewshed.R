test_that("runs correctly", {

  #load files
  test_dsm <- raster::raster(system.file("tests\\testthat\\test_dsm.tif",
                                         package ="viewscape"))

  test_viewpoint <- sf::read_sf(system.file("tests\\testthat\\test_viewpoint.shp",
                                               package = "viewscape"))


  test_viewpoint <- sf::st_coordinates(test_viewpoint)

  test_viewpoint <- c(test_viewpoint[,1], test_viewpoint[,2])

  #run function
  test_function <- viewscape::calculate_viewshed(dsm = test_dsm,
                                                 viewpoint = test_viewpoint)

  expect_output(str(test_function), "data.frame")

})

#raster::plot(test_dsm)

#plot(test_viewpoint, pch=16, col="red", add=TRUE)

#test_sf <- sf::st_as_sf(test_function, coords=c("x","y"),
#crs=4326)

#plot(test_sf, pch=16, col="blue", add=TRUE)
