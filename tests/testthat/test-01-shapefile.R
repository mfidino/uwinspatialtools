testthat::context("Test extract_polygon")

testthat::test_that(
  "extract_polygon",
  {
    library(sf)
    nc <- sf::st_read(
      system.file("shape/nc.shp", package="sf"),
      quiet = TRUE
    )

    nc <- sf::st_transform(
      nc,
      crs = 32617
    )
    bounds <- sf::st_bbox(nc)
    ncg <- sf::st_geometry(nc)
    cntrd <- sf::st_centroid(ncg)
    # randomly sample 5
    set.seed(663)
    cntrd <- cntrd[sample(1:length(cntrd), 5)]
    cndat <- sf::st_as_sf(
      data.frame(
      sites = as.character(1:5),
      cntrd
      )
    )
    # generate some points, with
    # some outside of the shapefile
    set.seed(555)
    mp <- data.frame(
      long = runif(
        5,
        bounds["xmin"],
        bounds["xmax"]
      ),
      lat = runif(
        5,
        bounds["ymin"],
        bounds["ymax"]
      )
    )
    mp <- sf::st_as_sf(
      mp,
      coords = c("long","lat"),
      crs = sf::st_crs(nc)
    )

    f <- function(my_points, location_column, my_buffer, my_shape,layers){
      extract_polygon(
        my_points = my_points,
        location_column = location_column,
        my_buffer = my_buffer,
        my_shape = my_shape,
        layers = layers
      )
    }
    testthat::expect_s3_class(
      f(cndat, "sites", 0.01, nc, c("AREA")),
      "data.frame"
    )
    testthat::expect_warning(
        f(cndat, "sites", 0.01, nc, c("AREA", "hotdog"))
    )
    testthat::expect_s3_class(
      f(cndat, "sites", 0.01, nc, NULL),
      "data.frame"
    )
    testthat::expect_s3_class(
      f(cndat, 1, 0.01, nc, "AREA"),
      "data.frame"
    )
    testthat::expect_error(
      f(cndat, "LocationName", 0.01, nc, "AREA")
    )
    testthat::expect_error(
      f(cndat, 1, 0.01, nc, nc)
    )
    testthat::expect_error(
      f(cndat, 1, 0.01, "burrito", "AREA")
    )
  }








)
