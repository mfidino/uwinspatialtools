testthat::context("Test extract_raster_prop")

testthat::test_that(
  "extract_raster_prop",
  {
    library(terra)
    library(sf)
    set.seed(444)
    mat <- matrix(
      sample(1:10,100^2, TRUE),
      100,
      100
    )
    mat <- terra::rast(
      mat
    )
    terra::crs(mat) <- "+init=epsg:32616"

    sites <- data.frame(
      names = LETTERS[1:20],
      x = sample(seq(0.2,0.8,0.01), 20),
      y = sample(seq(0.2,0.8,0.01), 20)
    )
    sites <- sf::st_as_sf(
      sites,
      coords = c("x","y"),
      crs = 32616
    )

    f <- function(my_points, location_column, my_buffer, my_raster_data,lulc_cats){
      extract_raster_prop(
        my_points = my_points,
        location_column = location_column,
        my_buffer = my_buffer,
        my_raster_data = my_raster_data,
        lulc_cats = lulc_cats
      )
    }
    testthat::expect_s3_class(
      f(
        my_points = sites,
        location_column = "names",
        my_buffer = 10,
        my_raster_data = mat,
        lulc_cats = list(forest = c(1,2))
      ),
      "data.frame"
    )
    testthat::expect_s3_class(
      f(
        my_points = sites,
        location_column = "names",
        my_buffer = 10,
        my_raster_data = mat,
        lulc_cats = list(forest = c(1,2),imperv = 3, urban = c(7:10))
      ),
      "data.frame"
    )
    testthat::expect_s3_class(
      f(
        my_points = sites,
        location_column = "names",
        my_buffer = 10,
        my_raster_data = mat,
        lulc_cats = list(forest = c(1,2))
      ),
      "data.frame"
    )
    testthat::expect_s3_class(
      f(
        my_points = sites,
        location_column = "names",
        my_buffer = 10,
        my_raster_data = mat,
        lulc_cats = NULL
      ),
      "data.frame"
    )
    testthat::expect_s3_class(
      f(
        my_points = sites,
        location_column = 1,
        my_buffer = 10,
        my_raster_data = mat,
        lulc_cats = 1:3
      ),
      "data.frame"
    )
    testthat::expect_s3_class(
      f(
        my_points = sites,
        location_column = "names",
        my_buffer = 10,
        my_raster_data = mat,
        lulc_cats = 1:3
      ),
      "data.frame"
    )

    testthat::expect_error(
      f(
        my_points = sites,
        location_column = "name",
        my_buffer = 10,
        my_raster_data = mat,
        lulc_cats = NULL
      )
    )
    testthat::expect_error(
      f(
        my_points = sites,
        location_column = "names",
        my_buffer = 10,
        my_raster_data = mat,
        lulc_cats = 11
      )
    )

  }








)
