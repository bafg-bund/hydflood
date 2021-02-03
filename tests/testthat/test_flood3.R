library(testthat)
options("rgdal_show_exportToProj4_warnings" =  "none")
library(hydflood)

context("flood3")

test_that("Elbe", {
    
    # Elbe
    hf <- Sys.getenv("hydflood")
    x <- hydRasterStack(filename_dem = paste0(hf, "/data-raw/raster.dem.tif"), 
                        filename_csa = paste0(hf, "/data-raw/raster.csa.tif"))
    
    # create a temporal sequence
    seq <- seq(as.Date("2016-12-01"), as.Date("2016-12-31"), by = "day")
    
    # compute a flood duration
    fd <- flood3(x = x, seq = seq)
    
    expect_equal(class(fd)[1], "RasterLayer")
    expect_equal(maxValue(fd), 31)
    expect_equal(minValue(fd), 0)
    expect_equal(fd@data@attributes[[1]][1], 
                 paste0("flood duration computed by hydflood::flood3() for ",
                        "the following temporal sequence of type 'Date' with ",
                        "length 31:"))
    
    # errors
    # x missing
    expect_error(flood3(seq = seq), "The 'x' argument has to be supplied.")
    
    # x class
    expect_error(flood3(x = x$dem, seq = seq), 
                 "'x' must be type 'hydRasterStack'")
    
    # seq missing
    expect_error(flood3(x), "The 'seq' argument has to be supplied.")
    
    # seq class
    expect_error(flood3(x, c("a", "b")), 
                 "'seq' must be either type 'Date' or c('POSIXct', 'POSIXt').",
                 fixed = TRUE)
    
    # seq length
    expect_error(flood3(x, numeric()), 
                        "'seq' must have length larger 0.")
    
    # seq NA
    expect_error(flood3(x, as.Date(NA)), 
                 "'seq' or elements of it must not be NA.")
    
    # seq range Date
    seq <- seq(as.Date("1988-12-21"), as.Date("1988-12-22"), by = "days")
    expect_error(flood3(x, seq), 
                 "'seq' must be between 1990-01-01 and yesterday")
    seq <- seq(as.Date("2050-12-21"), as.Date("2050-12-22"), by = "days")
    expect_error(flood3(x, seq), 
                 "'seq' must be between 1990-01-01 and yesterday")
    
    # seq range POSIX 
    now <- Sys.time()
    seq <- c(now - 3600, now - 3600 * 2)
    expect_equal(class(flood3(x, seq))[1], "RasterLayer")
    
    months_ago <- now - 3600 * 24 * 30 * 2
    seq <- c(months_ago - 3600, months_ago - 3600 * 2)
    expect_error(flood3(x, seq), 
                 "00:00:00 and now, if type of 'seq' is c('POSIXct', 'POSIXt')",
                 fixed = TRUE)
    
    # filename class
    expect_error(flood3(x, seq, 1), "'filename' must be type 'character'.")
    
    # filename length
    expect_error(flood3(x, seq, c("a", "b")), 
                 "'filename' must have length 1.")
    
})


test_that("Rhein", {
    
    # Rhein
    hf <- Sys.getenv("hydflood")
    x <- hydRasterStack(filename_dem = paste0(hf, 
                                       "/data-raw/raster.dem_plittersdorf.tif"), 
                        filename_csa = paste0(hf, 
                                       "/data-raw/raster.csa_plittersdorf.tif"))
    
    # create a temporal sequence
    seq <- seq(as.Date("2016-12-01"), as.Date("2016-12-31"), by = "day")
    
    # compute a flood duration
    fd <- flood3(x = x, seq = seq)
    
    expect_equal(class(fd)[1], "RasterLayer")
    expect_equal(maxValue(fd), 31)
    expect_equal(minValue(fd), 0)
    expect_equal(fd@data@attributes[[1]][1], 
                 paste0("flood duration computed by hydflood::flood3() for ",
                        "the following temporal sequence of type 'Date' with ",
                        "length 31:"))
    
    # errors
    # x missing
    expect_error(flood3(seq = seq), "The 'x' argument has to be supplied.")
    
    # x class
    expect_error(flood3(x = x$dem, seq = seq), 
                 "'x' must be type 'hydRasterStack'")
    
    # overlap of x
    #crs(y) <- sp::CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")
    #crs(y) <- sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")
    #expect_error(flood3(x = y, seq = seq), 
    
    # seq missing
    expect_error(flood3(x), "The 'seq' argument has to be supplied.")
    
    # seq class
    expect_error(flood3(x, c("a", "b")), 
                 "'seq' must be either type 'Date' or c('POSIXct', 'POSIXt').",
                 fixed = TRUE)
    
    # seq length
    expect_error(flood3(x, numeric()), 
                 "'seq' must have length larger 0.")
    
    # seq NA
    expect_error(flood3(x, as.Date(NA)), 
                 "'seq' or elements of it must not be NA.")
    
    # seq range Date
    seq <- seq(as.Date("1988-12-21"), as.Date("1988-12-22"), by = "days")
    expect_error(flood3(x, seq), 
                 "'seq' must be between 1990-01-01 and yesterday")
    seq <- seq(as.Date("2050-12-21"), as.Date("2050-12-22"), by = "days")
    expect_error(flood3(x, seq), 
                 "'seq' must be between 1990-01-01 and yesterday")
    
    # seq range POSIX 
    now <- Sys.time()
    seq <- c(now - 3600, now - 3600 * 2)
    expect_equal(class(flood3(x, seq))[1], "RasterLayer")
    
    months_ago <- now - 3600 * 24 * 30 * 2
    seq <- c(months_ago - 3600, months_ago - 3600 * 2)
    expect_error(flood3(x, seq), 
                 "00:00:00 and now, if type of 'seq' is c('POSIXct', 'POSIXt')",
                 fixed = TRUE)
    
    # filename class
    expect_error(flood3(x, seq, 1), "'filename' must be type 'character'.")
    
    # filename length
    expect_error(flood3(x, seq, c("a", "b")), 
                 "'filename' must have length 1.")
    
})
