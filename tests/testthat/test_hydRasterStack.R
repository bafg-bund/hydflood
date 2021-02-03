library(testthat)
options("rgdal_show_exportToProj4_warnings" =  "none")
library(hydflood)

context("hydRasterStack")

wgs <- sp::CRS(SRS_string = 'GEOGCRS["WGS 84",
    DATUM["World Geodetic System 1984",
        ELLIPSOID["WGS 84",6378137,298.257223563,
            LENGTHUNIT["metre",1]],
        ID["EPSG",6326]],
    PRIMEM["Greenwich",0,
        ANGLEUNIT["degree",0.0174532925199433],
        ID["EPSG",8901]],
    CS[ellipsoidal,2],
        AXIS["longitude",east,
            ORDER[1],
            ANGLEUNIT["degree",0.0174532925199433,
                ID["EPSG",9122]]],
        AXIS["latitude",north,
            ORDER[2],
            ANGLEUNIT["degree",0.0174532925199433,
                ID["EPSG",9122]]],
    USAGE[
        SCOPE["unknown"],
        AREA["World."],
        BBOX[-90,-180,90,180]]]')

test_that("General tests", {
    
    # smaller extent (crop)
    if (Sys.info()["nodename"] == "r.bafg.de") {
        # input data
        filename_dem <- paste0("/home/WeberA/freigaben/U/U3/Auengruppe_INFORM/",
                               "RH_336_867_UFD/data/ascii/r002_PLITTERSDORF1_D",
                               "EM.asc")
        filename_csa <- paste0("/home/WeberA/freigaben/U/U3/Auengruppe_INFORM/",
                               "RH_336_867_UFD/data/ascii/r002_PLITTERSDORF1_C",
                               "SA.asc")
        ext <- extent(436500, 438000, 5415000, 5416500)
        
        # tests
        expect_message(a <- hydRasterStack(filename_dem, filename_csa, ext,
                                           utm32n),
                       "'ext' will be used to crop the supplied raster file(s).",
                       fixed = TRUE)
        expect_message(b <- hydRasterStack(filename_csa = filename_csa, 
                                           ext = ext, crs = utm32n),
                       "'ext' will be used to crop the supplied raster file",
                       fixed = TRUE)
        expect_equal(extent(hydRasterStack(ext = ext, crs = utm32n)), ext)
        expect_equal(crs(hydRasterStack(ext = ext, crs = utm32n)), utm32n)
    }
    
    # the same extents and crs, but different data sources
    hf3 <- Sys.getenv("hydflood")
    filename_dem <- paste0(hf3, "/data-raw/raster.dem.tif")
    filename_csa <- paste0(hf3, "/data-raw/raster.csa.tif")
    ext_csa <- raster::extent(raster(filename_csa))
    crs_csa <- raster::crs(raster(filename_csa))
    expect_equal(dim(hydRasterStack(filename_dem = filename_dem, 
                                    filename_csa = filename_csa)),
                 c(1000, 1000, 2))
    expect_equal(res(hydRasterStack(filename_dem = filename_dem, 
                                    filename_csa = filename_csa)),
                 c(1,1))
    
    # execute the folowing check ony on r.bafg.de, since the DEM data are not 
    # public yet
    if (Sys.info()["nodename"] == "r.bafg.de") {
        expect_equal(dim(hydRasterStack(filename_csa = filename_csa)),
                     c(1000, 1000, 2))
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
        # expect_equal(a, b)
        expect_message(c <- hydRasterStack(tmp_dem1, tmp_csa1,
                                           extent(309000, 310000,
                                                  5749000, 5749500)),
                       "'ext' will be used to crop the supplied raster file(s)",
                       fixed = TRUE)
        tmp_dem2 <- rasterTmpFile(prefix = "r_test_dem_")
        tmp_dem3 <- rasterTmpFile(prefix = "r_test_dem_")
        tmp_csa2 <- rasterTmpFile(prefix = "r_test_csa_")
        writeRaster(c$dem, tmp_dem2)
        writeRaster(raster::aggregate(raster(tmp_csa1), fact = 2), tmp_csa2,
                    overwrite = TRUE)
        d <- c$dem
        crs(d) <- "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs"
        writeRaster(d, tmp_dem3)
        
        expect_error(hydRasterStack(filename_dem = tmp_dem2,
                                    filename_csa = tmp_csa1),
                     "extents of 'filename_dem' and 'filename_csa' have to mat")
        expect_error(hydRasterStack(filename_dem = tmp_dem2,
                                    filename_csa = tmp_csa2),
                     "extents of 'filename_dem' and 'filename_csa' have to mat")
        expect_error(hydRasterStack(filename_dem = tmp_dem2,
                                    filename_csa = tmp_csa2),
                     "resolutions of 'filename_dem' and 'filename_csa' have to")
        expect_error(hydRasterStack(filename_dem = tmp_dem3,
                                    filename_csa = tmp_csa2),
                     "coordinate reference systems of 'filename_dem' and 'file")
        expect_error(hydRasterStack(ext = extent(d)),
                     "The 'crs' argument has to be supplied.")
        expect_error(hydRasterStack(ext = extent(d), crs = 1),
                     "'crs' must be type 'CRS'")
        expect_error(hydRasterStack(filename_dem = tmp_dem1,
                                    filename_csa = tmp_csa1,
                                    crs = wgs),
                     "supplied 'crs' does not agree with the crs supplied through")
        expect_error(hydRasterStack(ext = extent(d), crs = wgs),
                     "crs must be either 'ETRS 1989 UTM 32N' or 'ETRS 1989 UTM 33N")
        expect_error(hydRasterStack(ext = extent(200000, 201000, 5749000, 5749500),
                                    crs = crs_csa),
                     "ea does NOT overlap with the active floodplain of River Elbe")
        expect_error(hydRasterStack(ext = extent(200000, 201000, 5749000, 5749500),
                                    crs = utm32n),
                     "ea does NOT overlap with the active floodplain of River Rhin")
        
        unlink(tmp_dem1)
        unlink(tmp_dem2)
        unlink(tmp_dem3)
        unlink(tmp_csa1)
        unlink(tmp_csa2)
    }
    
    # input data files
    expect_error(hydRasterStack(c(1,2,3)), 
                 "'filename_dem' must be type 'character'")
    expect_error(hydRasterStack(c(1,2,3)), 
                 "'filename_dem' must have length 1")
    expect_error(hydRasterStack(filename_csa = c(1,2,3)),
                 "'filename_csa' must be type 'character'")
    expect_error(hydRasterStack(filename_csa = c(1,2,3)),
                 "'filename_csa' must have length 1")
    
})

