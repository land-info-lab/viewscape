context("Get_extent")

test_that("runs correctly", {

  #load files
  test_dsm <- raster::raster(system.file("test_data/test_dsm.tif",
                                         package ="viewscape"))

  #raster::plot(test_dsm)

  test_viewpoint <- sf::read_sf(system.file("test_data/test_viewpoint.shp",
                                            package = "viewscape"))

  #plot(test_viewpoint, pch=16, col="red", add=TRUE)

  test_viewpoint <- sf::st_coordinates(test_viewpoint)

  test_viewpoint <- c(test_viewpoint[,1], test_viewpoint[,2])

  #run function
  test_visiblepoint <- viewscape::calculate_viewshed(dsm = test_dsm,
                                                     viewpoint = test_viewpoint)

  #plot(test_sf, pch=16, col="blue", add=TRUE)

  test_extent <- viewscape::get_extent(test_dsm, test_visiblepoint)
  expect_type(test_extent, "double")
})
