library(testthat)
library(hydflood)

context("getDEM")

test_that("General tests", {
    
    c32 <- CRS(paste0("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,",
                      "0 +units=m +no_defs"))
    c33 <- CRS(paste0("+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,",
                      "0 +units=m +no_defs"))
    wgs <- CRS("+init=epsg:4326")
    
    # input data checks
    expect_error(getDEM(), 
                 paste0("Error 1: If you don't provide an existing 'filename',",
                        " you have to specify 'ext' and 'crs'."))
    expect_error(getDEM(c(1,2,3)), "'filename' must be type 'character'")
    expect_error(getDEM(c(1,2,3)), "'filename' must have length 1")
    expect_error(getDEM(ext = c(1,2,3)), "ovide a CRS, you must specify 'crs'.")
    expect_error(getDEM(ext = c(1,2,3), crs = c32), "'ext' must be type 'Extent'")
    expect_error(getDEM(ext = extent(1,2,3,4), crs = c32),
                 "'ext' does NOT overlap with the active floodplain of River R")
    expect_error(getDEM(ext = extent(1,2,3,4), crs = c33),
                 "'ext' does NOT overlap with the active floodplain of River E")
    expect_error(getDEM(ext = extent(1,2,3,4), crs = "c"),
                 "'crs' must be type 'CRS'.")
    expect_error(getDEM(ext = extent(1,2,3,4), crs = wgs),
                 "'crs' must be either 'ETRS 1989 UTM 32N' or")
    
    # smaller extent (crop)
    if (Sys.info()["nodename"] == "r.bafg.de") {
        
        # input data
        filename <- paste0("/home/WeberA/freigaben/U/U3/Auengruppe_INFORM/RH_3",
                           "36_867_UFD/data/tiff/r002_PLITTERSDORF1_DEM.tif")
        ext <- extent(436500, 438000, 5415000, 5416500)
        
        # tests
        d <- getDEM(filename = filename)
        
        expect_message(getDEM(filename = filename, ext = ext),
                       "'ext' will be used to crop the supplied raster file.",
                       fixed = TRUE)
        expect_message(getDEM(filename = filename, ext = ext, crs = c32),
                       "'ext' will be used to crop the supplied raster file.",
                       fixed = TRUE)
        expect_error(getDEM(filename = filename, ext = ext, crs = c33),
                       "does not agree with the crs of the raster suppl",
                       fixed = TRUE)
        
        tmp_dem1 <- rasterTmpFile(prefix = "r_test_dem_")
        if (file.exists(tmp_dem1)) {unlink(tmp_dem1)}
        d <- getDEM(filename = tmp_dem1, ext = ext, crs = c32)
        
        expect_equal(file.exists(tmp_dem1), TRUE)
        expect_equal(extent(d), ext)
        expect_equal(crs(d), c32)
        
        unlink(tmp_dem1)
    }
    
    # the same extents and crs, but different data sources
    hf3 <- Sys.getenv("hydflood")
    filename <- paste0(hf3, "/data-raw/raster.dem.tif")
    d <- getDEM(filename = filename)
    
    expect_equal(dim(d), c(1000, 1000, 1))
    expect_equal(res(d), c(1, 1))
    
    expect_error(getDEM(filename, ext = extent(308000, 310000,
                                               5749000, 5750000)),
                 "'ext' must be totally within the raster")
    
    expect_message(getDEM(filename, ext = extent(309200, 310000,
                                                 5749000, 5750000)),
                   "'ext' will be used to crop the supplied raster file.")
    expect_error(getDEM(ext = extent(295000, 340000, 5744000, 5753000),
                        crs = c33),
                 "'ext' is very large and covers more than 5 ")
    expect_warning(getDEM(ext = extent(300000, 330000, 5744000, 5753000),
                        crs = c33),
                 "'ext' is very large and covers more than 3 ")
    
})

