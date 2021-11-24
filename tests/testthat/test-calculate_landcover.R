test_that("runs correctly", {

  #load files

  defaultW <- getOption("warn") #suppress read raster warning

  options(warn = -1)

  test_dsm <- raster::raster(system.file("test_data/test_dsm.tif",
                                         package ="viewscape"))

  test_landcover <- raster::raster(system.file("test_data/test_landcover.tif",
                                            package ="viewscape"))

  options(warn = defaultW) #turn warnings back on

  test_viewpoint <- sf::read_sf(system.file("test_data/test_viewpoint.shp",
                                            package = "viewscape"))

  test_viewpoint <- sf::st_coordinates(test_viewpoint)

  test_viewpoint <- c(test_viewpoint[,1], test_viewpoint[,2])

  # get visible points based on viewpoint
  test_visiblepoint <- viewscape::calculate_viewshed(dsm = test_dsm,
                                                     viewpoint = test_viewpoint)

  #run function
  # in the sample data of land cover, value 2 is for vegetation and value 4 is for imperviousness
  test_landcover_area <- viewscape::calculate_landcover(landcover = test_landcover, dsm = test_dsm,
                                                        visiblepoints = test_visiblepoint,
                                                        vegetation = 2, imperviousness = 4)

  expect_type(test_landcover_area, "double")
})
