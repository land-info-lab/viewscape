testthat::test_that("runs correctly", {

  las <- viewscape::get_lidar(x = -83.741289,
                              y = 42.270146,
                              r = 1000,
                              epsg = 2253,
                              plot = FALSE)

  testthat::expect_type(las, "character")
})
