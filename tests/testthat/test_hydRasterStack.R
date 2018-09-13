library(testthat)
library(sp)
library(raster)
library(hydflood3)


context("hydRasterStack")

test_that("General tests", {
    
    # smaller exteent (crop)
    if (Sys.info()["nodename"] == "hpc-service") {
        # input data
        filename_dem <- paste0("/home/WeberA/freigaben/U/U2/RH_336_867_UFD/da",
                               "ta/ascii/r002_PLITTERSDORF1_DEM.asc")
        filename_csa <- paste0("/home/WeberA/freigaben/U/U2/RH_336_867_UFD/da",
                               "ta/ascii/r002_PLITTERSDORF1_CSA.asc")
        ext <- extent(436500, 438000, 5415000, 5416500)
        crs <- crs("+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs", 
                   asText = FALSE)
        
        # tests
        expect_message(a <- hydRasterStack(filename_dem, filename_csa, ext, crs),
                       "'ext' will be used to crop the supplied raster file(s).",
                       fixed = TRUE)
        expect_message(b <- hydRasterStack(filename_csa = filename_csa, 
                                           ext = ext, crs = crs),
                       "'ext' will be used to crop the supplied raster file",
                       fixed = TRUE)
        expect_equal(extent(hydRasterStack(ext = ext, crs = crs)), ext)
        expect_equal(crs(hydRasterStack(ext = ext, crs = crs)), crs)
    }
    
    # the same extents and crs, but different data sources
    filename_dem <- "~/hydflood3/data-raw/raster.dem.tif"
    filename_csa <- "~/hydflood3/data-raw/raster.csa.tif"
    ext_csa <- extent(raster(filename_csa))
    crs_csa <- crs(raster(filename_csa))
    expect_equal(dim(hydRasterStack(filename_dem = filename_dem, 
                                    filename_csa = filename_csa)),
                 c(1000, 1000, 2))
    expect_equal(res(hydRasterStack(filename_dem = filename_dem, 
                                    filename_csa = filename_csa)),
                 c(1,1))
    expect_equal(dim(hydRasterStack(filename_csa = filename_csa)),
                 c(1000, 1000, 2))
    expect_equal(minValue(hydRasterStack(filename_dem = filename_dem, 
                                         filename_csa = filename_csa)), 
                 minValue(hydRasterStack(filename_csa = filename_csa)))
    expect_equal(minValue(hydRasterStack(filename_dem = filename_dem, 
                                         filename_csa = filename_csa)), 
                 minValue(hydRasterStack(filename_dem = filename_dem)))
    expect_equal(maxValue(hydRasterStack(filename_dem = filename_dem, 
                                         filename_csa = filename_csa)), 
                 maxValue(hydRasterStack(filename_csa = filename_csa)))
    expect_equal(maxValue(hydRasterStack(filename_dem = filename_dem, 
                                         filename_csa = filename_csa)), 
                 maxValue(hydRasterStack(ext = ext_csa,
                                         crs = crs_csa)))
    
    # store files
    tmp_dem1 <- rasterTmpFile(prefix = "r_test_dem_")
    tmp_csa1 <- rasterTmpFile(prefix = "r_test_csa_")
    a <- hydRasterStack(filename_dem = tmp_dem1, filename_csa = tmp_csa1, 
                        ext = ext_csa, crs = crs_csa)
    expect_equal(file.exists(tmp_dem1), TRUE)
    expect_equal(file.exists(tmp_csa1), TRUE)
    b <- hydRasterStack(filename_dem = tmp_dem1, filename_csa = tmp_csa1, 
                        ext = ext_csa, crs = crs_csa)
    expect_equal(a, b)
    expect_equal(minValue(hydRasterStack(filename_dem = filename_dem,
                                         filename_csa = tmp_csa1)), 
                 minValue(hydRasterStack(filename_dem = tmp_dem1,
                                         filename_csa = filename_csa)))
    expect_message(c <- hydRasterStack(tmp_dem1, tmp_csa1, 
                                       extent(309000, 310000, 
                                              5749000, 5749500)),
                   "'ext' will be used to crop the supplied raster file(s).",
                   fixed = TRUE)
    tmp_dem2 <- rasterTmpFile(prefix = "r_test_dem_")
    tmp_dem3 <- rasterTmpFile(prefix = "r_test_dem_")
    tmp_csa2 <- rasterTmpFile(prefix = "r_test_csa_")
    writeRaster(c$dem, tmp_dem2)
    writeRaster(aggregate(raster(tmp_csa1), fact = 2), tmp_csa2)
    d <- c$dem
    crs(d) <- "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs"
    writeRaster(d, tmp_dem3)
    
    # input data files
    expect_error(hydRasterStack(c(1,2,3)), 
                 "'filename_dem' must be type 'character'")
    expect_error(hydRasterStack(c(1,2,3)), 
                 "'filename_dem' must have length 1")
    expect_error(hydRasterStack(filename_csa = c(1,2,3)),
                 "'filename_csa' must be type 'character'")
    expect_error(hydRasterStack(filename_csa = c(1,2,3)),
                 "'filename_csa' must have length 1")
    
    expect_error(hydRasterStack(filename_dem = tmp_dem2,
                                filename_csa = tmp_csa1), 
                 "extents of 'filename_dem' and 'filename_csa' have to match.")
    expect_error(hydRasterStack(filename_dem = tmp_dem2,
                                filename_csa = tmp_csa2), 
                 "extents of 'filename_dem' and 'filename_csa' have to match.")
    expect_error(hydRasterStack(filename_dem = tmp_dem2,
                                filename_csa = tmp_csa2), 
                 "resolutions of 'filename_dem' and 'filename_csa' have to ma")
    expect_error(hydRasterStack(filename_dem = tmp_dem3,
                                filename_csa = tmp_csa2),
                 "coordinate reference systems of 'filename_dem' and 'filename")
    expect_error(hydRasterStack(ext = extent(d)), 
                 "The 'crs' argument has to be supplied.")
    expect_error(hydRasterStack(ext = extent(d), crs = 1), 
                 "'crs' must be type 'CRS'")
    expect_error(hydRasterStack(filename_dem = tmp_dem1, 
                                filename_csa = tmp_csa1,
                                crs = CRS("+init=epsg:4326")),
                 "supplied 'crs' does not agree with the crs supplied through")
    expect_error(hydRasterStack(ext = extent(d), crs = CRS("+init=epsg:4326")),
                 "crs must be either 'ETRS 1989 UTM 32N' or 'ETRS 1989 UTM 33N")
    expect_error(hydRasterStack(ext = extent(200000, 201000, 5749000, 5749500), 
                                crs = crs_csa),
                 "ea does NOT overlap with the active floodplain of River Elbe")
    expect_error(hydRasterStack(ext = extent(200000, 201000, 5749000, 5749500), 
                                crs = CRS(paste0("+proj=utm +zone=32 +ellps=GR",
                                                 "S80 +units=m +no_defs"))),
                 "ea does NOT overlap with the active floodplain of River Rhin")
    
    unlink(tmp_dem1)
    unlink(tmp_dem2)
    unlink(tmp_dem3)
    unlink(tmp_csa1)
    unlink(tmp_csa2)
    
})







