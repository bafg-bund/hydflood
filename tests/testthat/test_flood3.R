library(testthat)
library(sp)
library(raster)
library(hydflood3)

context("flood3")

test_that("x", {
    
    # Elbe
    #csa <- raster(system.file("data-raw/raster.csa.tif", package = "hydflood3"))
    #dem <- raster(system.file("data-raw/raster.dem.tif", package = "hydflood3"))
    csa <- raster("~/hydflood3/data-raw/raster.csa.tif")
    dem <- raster("~/hydflood3/data-raw/raster.dem.tif")
    
    x <- stack(csa, dem)
    names(x) <- c("csa", "dem")
    
    # create a temporal sequence
    seq <- seq(as.Date("2016-12-01"), as.Date("2016-12-31"), by = "day")
    
    # compute a flood duration
    fd <- flood3(x = x, seq = seq)
    
    expect_equal(class(fd)[1], "RasterLayer")
    expect_equal(maxValue(fd), 31)
    expect_equal(minValue(fd), 0)
    expect_equal(fd@data@attributes[[1]][1], 
                 paste0("flood duration computed by hydflood3::flood3() for ",
                        "the following temporal sequence of type 'Date' with ",
                        "length 31:"))
    
    # errors
    # x missing
    expect_error(flood3(seq = seq), "The 'x' argument has to be supplied.")
    
    # x class
    expect_error(flood3(x = dem, seq = seq), "'x' must be type 'RasterStack'")
    expect_error(flood3(x = dem, seq = seq), 
                 "names(x)[1:2] must be c('csa', 'dem')",
                 fixed = TRUE)
    
    # names of x
    y <- x
    names(y) <- c("a", "b")
    expect_error(flood3(x = y, seq = seq), 
                 "names(x)[1:2] must be c('csa', 'dem')",
                 fixed = TRUE)
    
    # crs of x
    names(y) <- c("csa", "dem")
    crs(y) <- sp::CRS("+init=epsg:25831")
    expect_error(flood3(x = y, seq = seq), 
                 "x must be either 'ETRS 1989 UTM 32N' or 'ETRS 1989 UTM 33N'.")
    
    #crs(y) <- sp::CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m")
    #crs(y) <- sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +units=m")
    #expect_error(flood3(x = y, seq = seq), 
    # identicalCRS(x, y) is not TRUE
    
    # overlap of x
    
    # seq missing
    expect_error(flood3(x), "The 'seq' argument has to be supplied.")
    
    # seq class
    
    # seq length
    
    # seq NA
    
    # seq range Date
    
    # seq range POSIX
    
    # filename class
    expect_error(flood3(x, seq, 1), "'filename' must be type 'character'.")
    
    # filename length
    expect_error(flood3(x, seq, c("a", "b")), 
                 "'filename' must have length 1.")
    
})
