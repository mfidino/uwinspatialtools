##############################################
#
# Example of extracting from a shapefile
#
# Written by T. Gallo and M. Fidino
#
##############################################

library(uwinspatialtools)

# Need a table of site locations
# Here I use a random sample of 10 Chicago sites as an example
# Data avaliable in same github repo
# Columns: LocationName, UTM_E, UTM_N, UTMZone, City
site_coords <- read.csv(
  "./examples/2019-04-25_SampleChicagoSites.csv",
  stringsAsFactors = FALSE
)

# Create spatial points
#  You must put the correct coordinate reference system (CRS)
#  the respective city. For some extra infromation check out
#  browseURL("https://mgimond.github.io/Spatial/coordinate-systems-in-r.html")
sites <- sf::st_as_sf(
  site_coords,
  coords = c("UTM_E", "UTM_N"),
  crs = 26916
)


# Load 2010 statewide census data for housing and population
# Data can be downloaded from:
# browseURL("http://silvis.forest.wisc.edu/data/housing-block-change/")
# REPLACE FILE PATH AND LAYER NAME WITH LOCAL FILE PATH
pop_data <- sf::st_read(
  "D:/GIS/housing_density",
  layer = "il_blk10_Census_change_1990_2010_PLA2"
)

# fix any potential issules with the vectors before trying
#  to summarise.
pop_data <- sf::st_make_valid(pop_data)

# Run function to calculate housing units, housing density, population
#  and population density.  For this example we extract population data
#  within a 1km radius buffer.

population_data <- extract_polygon(
  my_points = sites,
  location_column = "LocationName",
  my_buffer = 1000,
  my_shape = pop_data,
  layers = c("HU10")
)
