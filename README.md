
# uwinspatialtools

---


<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/mfidino/uwinspatialtools/branch/main/graph/badge.svg)](https://codecov.io/gh/mfidino/uwinspatialtools?branch=main)
[![R build status](https://github.com/mfidino/uwinspatialtools/workflows/R-CMD-check/badge.svg)](https://github.com/mfidino/uwinspatialtools/actions)
<!-- badges: end -->
`uwinsspatialtools` is an R package, that at the moment, has two functions, and so only does two (albeit common) things:

1. Calculates the proportion of different landcover classes from a landcover
raster image within some user specified buffer around a set of locations. The 
function that does this is called `extract_raster_prop()`.

2. Calculates the average value per unit<sup>2</sup> from a shapefile of different
layers of a shapefile that intersect some user specified buffer around a set
or locations. The function that does this is called `extract_polygon()`.

This package is a starting point for a suite of common spatial analyses that
I need to perform for the Urban Wildlife Information Network(UWIN, hence the package name). UWIN is  which is a partnership of researchers who use the wildlife monitoring protocols created at Lincoln Park Zoo's Urban Wildlife Institute to understand the ecology and behavior of urban species. By comparing data throughout the network, we can understand differences in animal behavior across regions and find patterns that remain consistent around the globe. And to do that, it makes
sense to start compiling spatial data for different analyses in a standard format.

## Status: Experimental, active developement.

---

This package is in active development. I'll likely just add in new functions
when I locate some pain points while working with it. That said, the current
two functions are stable and tested.

## Installation

---

`uwinspatialtools` can be installed from Github:

```R
install.packages("devtools")
devtools::install_github("mfidino/uwinspatialtools")

```

## Author Contributions

---

Mason Fidino is the sole contributing author of `uwinspatialtools`. If you
are interested in contributing, see [Contributor Guidelines](CONTRIBUTING.md).

