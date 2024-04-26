# extract land use landcover data from a raster

#' @title Extract the proportion of a landcover class buffered around a point
#'
#' @description Given a set of locations and a raster, this function will
#' calculate the proportion of each landcover class specified in the
#' argument \code{lulc_cats}.
#'
#' @param my_points a spatial points data.frame that contains the locations
#' you wish to summarize landcover data at.
#'
#' @param location_column the name of the column in \code{my_points} that
#'   specifies the names of each location. Can either be the column name
#'   or an integer that specifies it's column location.
#'
#' @param my_buffer The radius of a buffer around each point form which
#                           to extract cell values. If the distance between
#                           the sampling point and the center of a cell is
#                           less than or equal to the buffer, the cell is
#                           included. The buffer can be specified as a single
#                           value, or a vector of the length of the number of
#                           points. If the data are not projected (e.g.,
#                           latitude/longitude), unit should be in meters.
#                           Otherwise it should be in map units, which is also
#                           typically meters.
#'
#' @param my_raster_data A SpatRaster object that you want to extract data
#'                             from.
#'
#' @param lulc_cats Land-use land-cover categories.
#'                   If this is a numeric then the data will
#'                   be queried to return just those specific
#'                   lulc categories (e.g., lulc_cats = c(1,2))
#'                   will return the first two categories. If
#'                   this is a list then each element must be a
#'                   numeric vector. If the numeric vector is
#'                   > 1 for a specific list element then those
#'                   categories will be summed together. This
#'                   may be done to combine specific similar
#'                   category types (e.g., combine 'building',
#'                   'road', and 'other paved surfaces') to
#'                   generate an impervious surface category.
#'                   The numeric or the list can be named. If
#'                   they are then those names will be added
#'                   to the returned object. If NULL, then all
#'                   categories will be returned.
#'
#' @export


extract_raster_prop <- function(
  my_points,
  location_column,
  my_buffer,
  my_raster_data,
  lulc_cats = NULL
){

  sites <- my_points[,location_column]

cli::cli_h1("Reprojecting my_points to map projection")
  points_RP <- sf::st_transform(
    my_points,
    sf::st_crs(my_raster_data)
  )

  cli::cli_alert_success("my_points reprojected")

  points_RP <- sf::st_buffer(
    points_RP,
    dist = my_buffer
  )
  # Step 2.

  cli::cli_h1("Extracting spatial data")
  prop_extract <- suppressWarnings(
    exactextractr::exact_extract(
      my_raster_data,
      points_RP,
      fun="frac"
    )
  )



  # if lulc_cats is a list
  if(is.list(lulc_cats)){
    lulc_cats <- sapply(
      lulc_cats,
      function(x){
        paste0(
          "frac_",
          x
        )
      }
    )
    prop_extract <- apply(
      prop_extract,
      1,
      function(x){
        sapply(
          lulc_cats,
          function(y)
            ifelse(
              length(y) == 1,
              x[y],
              sum(x[y])
            )
        )
      }
    )
    if(length(lulc_cats) == 1){
      prop_extract <- t(t(prop_extract))
    }else{
      prop_extract <- t(prop_extract)
    }
  }

  # if it is a numeric
  if(is.numeric(lulc_cats)){
    lulc_cats <- paste0(
      "frac_", lulc_cats
    )
    prop_extract <- prop_extract[,lulc_cats]
  }
  # if it's a named list
  if(!is.null(names(lulc_cats)) & is.list(lulc_cats)){
      colnames(prop_extract) <- names(lulc_cats)
  }

  # create dataframe matching the sites with the extracted data
  df <- data.frame(
    LocationName = sites,
    prop_extract,
    row.names = NULL
  )
  # give the site column the same name as my_points
  if(is.character(location_column)){
    colnames(df)[1] <- location_column
  } else {
    colnames(df)[1] <- colnames(my_points)[location_column]
  }
  cli::cli_alert_success("Spatial data extracted")
  return(df[order(df[,1]),])
}



# extract data from polygons that intersect a given buffer

#' @title Summarise spatial polygon data around sites,
#'
#' @description Given a set of locations and a spatial polygons layer,
#'  this function xtract the median value from spatial polygons that intersect a
#' buffer around a site.
#'
#' @param location_column the name of the column in \code{my_points} that
#'   specifies the names of each location. Can either be the column name
#'   or an integer that specifies it's column location.
#'
#' @param my_points a spatial points data.frame that contains the locations
#' you wish to summarize landcover data at.
#'
#' @param location_column the name of the column in \code{my_points} that
#'   specifies the names of each location. Can either be the column name
#'   or an integer that specifies it's column location.
#'
#' @param my_buffer The radius of a buffer around each point form which
#                           to extract cell values. If the distance between
#                           the sampling point and the center of a cell is
#                           less than or equal to the buffer, the cell is
#                           included. The buffer can be specified as a single
#                           value, or a vector of the length of the number of
#                           points. If the data are not projected (e.g.,
#                           latitude/longitude), unit should be in meters.
#                           Otherwise it should be in map units, which is also
#                           typically meters.
#'
#' @param my_shape A \code{SpatialPolygonsDataFrame} object of a shape file you want to extract
#                           data from.
#' @param layers  A character vector of column names from my_shape
#'                          to summarise. Optional. If NULL, all numeric
#'                          columns are summarised.
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export


extract_polygon <- function(
  my_points,
  location_column,
  my_buffer,
  my_shape,
  layers = NULL
){
  if(is.numeric(location_column)){
    location_column <- colnames(my_points)[location_column]
  }
  # If an object is supplied to layers, make sure it is a character.
  warn <- NULL
  if(!is.null(layers)){
    if(!is.character(layers)){
      stop("A non-character object has been supplied to layers.")
    }
    # check if all the layers exist, warn if not.
    if(!all(layers %in% colnames(my_shape))){
      warn <- paste("\nWarning: 'my_shape' did not have all layers specified in",
                    "'layers' argument.\nMissing elements in 'layers' were",
                    "removed'.\n")
      # drop the elements in layer that lack columns in my_shape
      layers <- layers[which(layers %in% colnames(my_shape))]
      if(length(layers) == 0){
        layers <- NULL
      }
    }
  }
  # STEP 1
  # Reproject the point data to match projection of population layer
  cli::cli_h1("Reprojecting my_points to map projection")

  points_RP <- sf::st_transform(
    my_points,
    sf::st_crs(my_shape)
  )
  cli::cli_alert_success("my_points reprojected")

  # STEP 2 Create a buffer around each site
  cli::cli_h1("Extracting spatial data")

  points_buffer <- sf::st_buffer(
    points_RP,
    dist = my_buffer
  )
  my_shape$row <- 1:nrow(my_shape)

  # Extract population layer features that are contained within each buffers
  tmp1 <- sf::st_intersects(
    points_buffer,
    my_shape
  )
  tmp1 <- lapply(
    1:length(tmp1),
    function(y){
        my_shape[tmp1[[y]],]
    }
  )
  tmp1 <- dplyr::distinct(
    dplyr::bind_rows(
      tmp1
      )
  )
  tmp1$area <- sf::st_area(
    tmp1
  )
  shp_intersection <- suppressWarnings(
    sf::st_intersection(
      points_buffer,
      tmp1
    )
  )
  # get area of each intersection
  shp_intersection$ins_area <- sf::st_area(
    shp_intersection
  )

  shp_intersection$weight <- shp_intersection$ins_area /
    shp_intersection$area

  shp_intersection <- shp_intersection %>%
    dplyr::mutate_if(is.numeric, .funs = function(x) x * .$weight)


  # Summarise the data if layers is not null
  if(!is.null(layers)){
    summary_data <- shp_intersection %>%
      # remove spatial geometry so you are left with a data frame
      sf::st_set_geometry(NULL) %>%
      # group by Location Name
      dplyr::group_by(.data[[location_column]]) %>%
      # sum all the intersected pieces to get total housing units in each buffer
      dplyr::summarise_at(.vars = layers, .funs = sum) %>%
      # divide by area of buffer, converting to km^2
      #  spatial data are the clumns we are summarizing
      #  sf::st_area(points_buffer) is the area we are buffering
      #  we modify the units
      dplyr::mutate_if(is.numeric,
                       function(spatial_data){
                         spatial_data/units::set_units(
                           sf::st_area(
                             points_buffer
                           ),
                           "km^2"
                         )
                       }
      )  %>%
      dplyr::arrange(.data[[location_column]])

  } else {
    # locate numeric columns
    summary_data <- shp_intersection %>%
      # remove spatial geometry so you are left with a data frame
      sf::st_set_geometry(NULL) %>%
      # group by Location Name
      dplyr::group_by(.data[[location_column]]) %>%
      # sum all the intersected pieces to get total housing units in each buffer
      dplyr::summarise_if(is.numeric, .funs = sum) %>%
      # divide by area of buffer, converting to km^2
      #  spatial data are the clumns we are summarizing
      #  sf::st_area(points_buffer) is the area we are buffering
      #  we modify the units
      dplyr::mutate_if(is.numeric,
                       function(spatial_data){
                         spatial_data/units::set_units(
                           sf::st_area(
                             points_buffer
                           ),
                           "km^2"
                         )
                       }
      )

  }
  if(!is.null(warn)){
    warning(warn)
  }
  cli::cli_alert_success("Spatial data extracted")
  return(summary_data)
}


