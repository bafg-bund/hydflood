
<!-- README.md is generated from README.Rmd. Please edit that file -->
hydflood
=========

[![pipeline status](https://git.aqualogy.de/arnd/hydflood/badges/master/pipeline.svg)](https://git.aqualogy.de/arnd/hydflood/commits/master)

The R package **hydflood** is designed to compute flood extent and duration along German federal waterways Elbe and Rhine.

Installation
------------

**hydflood** is not currently available from CRAN, but you can install the development version from BfG's gitbucket server with:

``` r
install.packages("devtools")
library(devtools)
devtools::install_git("git://apps.bafg.de/gitbucket/webera/hydflood.git")
```

Usage
-----

The package **hydflood** is build around the packages `raster` and `hyd1d`.

``` r
# load the package
library(hydflood)

# import the raster data and create a raster stack
x <- hydRasterStack(filename_dem = "data-raw/raster.dem.tif",
                    filename_csa = "data-raw/raster.csa.tif")

# create a temporal sequence
seq <- seq(as.Date("2016-12-01"), as.Date("2016-12-31"), by = "day")

# compute a flood duration
fd <- flood3(x = x, seq = seq)

# and plot it
plot(fd)
```

<img src="README_files/figure-markdown_github/usage-1.png" style="display: block; margin: auto;" />
