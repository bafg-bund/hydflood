library(testthat)
library(sp)
library(raster)
library(hydflood3)

context("flood3")

test_that("x", {
    
    # Elbe
    #csa <- raster(system.file("data-raw/raster.csa.tif", package = "hydflood3"))
    #dem <- raster(system.file("data-raw/raster.dem.tif", package = "hydflood3"))
    csa <- raster("data-raw/raster.csa.tif")
    dem <- raster("data-raw/raster.dem.tif")
    
    x <- stack(csa, dem)
    names(x) <- c("csa", "dem")
    
    # create a temporal sequence
    seq <- seq(as.Date("2016-12-01"), as.Date("2016-12-31"), by = "day")
    
    # compute a flood duration
    fd <- flood3(x = x, seq = seq)
    
    expect_equal(class(fd)[1], "RasterLayer")
    expect_equal(maxValue(fd), 31)
    expect_equal(minValue(fd), 0)
    expect_equal(fd@data@attributes[[1]][1], "flood duration computed by hydflood3::flood3() for the following temporal sequence of type 'Date' with length 31:")
    
})
