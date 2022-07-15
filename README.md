
<!-- README.md is generated from README.Rmd. Please edit that file -->
hydflood
=========

The R package **hydflood** is designed to compute flood extents and durations 
along the German federal waterways Elbe and Rhine.

Installation
------------

**hydflood** is currently not available from CRAN, but you can install the 
developmental version from BfG's gitlab server with:

``` r
install.packages("devtools")
library(devtools)
devtools::install_git("git://gitlab.lan.bafg.de/auenoekologie/hydflood.git")
```

Usage
-----

The package **hydflood** is build around the packages `terra` and `hyd1d`.

``` r
# load the package
library(hydflood)

# import the raster data and create a raster stack
x <- hydSpatRaster(filename_dem = "data-raw/raster.dem.tif",
                   filename_csa = "data-raw/raster.csa.tif")

# create a temporal sequence
seq <- seq(as.Date("2016-12-01"), as.Date("2016-12-31"), by = "day")

# compute a flood duration
fd <- flood3(x = x, seq = seq)

# and plot it
plot(fd)
```

<img src="README_files/figure-markdown_github/usage-1.png" style="display: block; margin: auto;" />
