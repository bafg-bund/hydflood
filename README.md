
<!-- README.md is generated from README.Rmd. Please edit that file -->
hydflood3
=========

[![pipeline status](https://git.aqualogy.de/arnd/hydflood3/badges/master/pipeline.svg)](https://git.aqualogy.de/arnd/hydflood3/commits/master)

The R package **hydflood3** is designed to compute flood extent and duration along German federal waterways Elbe and Rhine.

Installation
------------

**hydflood3** is not currently available from CRAN, but you can install the development version from BfG's gitbucket server with:

``` r
install.packages("devtools")
library(devtools)
devtools::install_git("git://apps.bafg.de/gitbucket/webera/hydflood3.git")
```

Usage
-----

The package **hydflood3** is build around the packages `raster` and `hyd1d`.

``` r
# load the package
library(sp)
library(raster)
library(rgdal)
library(hyd1d)
library(hydflood3)

# import the raster data and create a raster stack
#csa <- raster(system.file("data-raw/raster.csa.tif"))
#dem <- raster(system.file("data-raw/raster.dem.tif"))
csa <- raster("data-raw/raster.csa.tif")
dem <- raster("data-raw/raster.dem.tif")
x <- stack(csa, dem)
names(x) <- c("csa", "dem")

# create a temporal sequence
seq <- seq(as.Date("2016-12-01"), as.Date("2016-12-31"), by = "day")

# compute a flood duration
fd <- flood3(x = x, seq = seq)

# and plot it
colfunc <- colorRampPalette(c("red","yellow","springgreen","royalblue"))
plot(fd, col = colfunc(length(seq)))
```

<img src="README_files/figure-markdown_github/usage-1.png" style="display: block; margin: auto;" />
