library(hydflood)

context("floodCharacteristicWaterlevel")

test_that("floodCharacteristicWaterlevel: checks", {
    
    ## x
    # x missing
    expect_error(floodCharacteristicWaterlevel(value = "MQ"),
                 "The 'x' argument has to be supplied.")
    
    # x class
    expect_error(floodCharacteristicWaterlevel(x = 1, value = "MQ"), 
                 "'x' must be type 'SpatRaster'")
    
    # names(x)
    y <- c(x = rast(crs = crs("EPSG:25833")),
           y = rast(crs = crs("EPSG:25833")))
    expect_error(floodCharacteristicWaterlevel(x = y, value = "MQ"),
                 "'names(x)' must be 'dem' and 'csa'.", fixed = TRUE)
    
    # crs(x)
    y <- rast(c(dem = rast(crs = crs("EPSG:25831")),
                csa = rast(crs = crs("EPSG:25831"))))
    expect_error(floodCharacteristicWaterlevel(x = y, value = "MQ"),
                 "The projection of x must be either", fixed = TRUE)
    
    ## value
    x <- rast(c(dem = rast(crs = crs("EPSG:25833")),
                csa = rast(crs = crs("EPSG:25833"))))
    
    # value missing
    expect_error(floodCharacteristicWaterlevel(x = x),
                 "'value' must be supplied.")
    
    # class(value)
    expect_error(floodCharacteristicWaterlevel(x = x, value = 1),
                 "'value' must be type 'character'.")
    
    # length(value)
    expect_error(floodCharacteristicWaterlevel(x = x, value = c("MQ", "MNQ")),
                 "The length of 'value' must 1.")
    
    # possible values
    expect_error(floodCharacteristicWaterlevel(x = x, value = "MTnw"),
                 "The supplied 'value' must be is among the foll", fixed = TRUE)
    
    ## df
    # class(df)
    expect_error(floodCharacteristicWaterlevel(x = x, value = "MQ", df = 1),
                 "'df' must be type 'data.frame'.")
    
    # class(value)
    expect_error(floodCharacteristicWaterlevel(x = x, value = 1,
                               df = data.frame()),
                 "'value' must be type 'character'.")
    
    # names(df)
    expect_error(floodCharacteristicWaterlevel(x = x, value = "MQ",
                               df = data.frame(a = 1)),
                 "\"gauging_station\" %in% names(df) is not TRUE",
                 fixed = TRUE)
    # class(df$gauging_station)
    expect_error(floodCharacteristicWaterlevel(x = x, value = "MQ",
                               df = data.frame(gauging_station = 1)),
                 "inherits(df$gauging_station, \"character\") is not TRUE",
                 fixed = TRUE)
    
    # names(df)
    expect_error(floodCharacteristicWaterlevel(x = x, value = "MQ",
                               df = data.frame(gauging_station = "test")),
                 "\"river\" %in% names(df) is not TRUE",
                 fixed = TRUE)
    # class(df$river)
    expect_error(floodCharacteristicWaterlevel(x = x, value = "MQ",
                               df = data.frame(gauging_station = "test",
                                               river = 1)),
                 "inherits(df$river, \"character\") is not TRUE",
                 fixed = TRUE)
    # df$river(s)
    expect_error(floodCharacteristicWaterlevel(x = x, value = "MQ",
                               df = data.frame(gauging_station = "test",
                                               river = "test")),
                 "all(tolower(unique(df$river)) %in% unique(",
                 fixed = TRUE)
    
    # names(df)
    expect_error(floodCharacteristicWaterlevel(x = x, value = "MQ",
                               df = data.frame(gauging_station = "test",
                                               river = "Elbe")),
                 "\"longitude\" %in% names(df) is not TRUE",
                 fixed = TRUE)
    # class(df$longitude)
    expect_error(floodCharacteristicWaterlevel(x = x, value = "MQ",
                               df = data.frame(gauging_station = "test",
                                               river = "Elbe",
                                               longitude = "a")),
                 "inherits(df$longitude, \"numeric\") | inherits(df$longitude,",
                 fixed = TRUE)
    
    # names(df)
    expect_error(floodCharacteristicWaterlevel(x = x, value = "MQ",
                               df = data.frame(gauging_station = "test",
                                               river = "Elbe",
                                               longitude = 1)),
                 "\"latitude\" %in% names(df) is not TRUE",
                 fixed = TRUE)
    # class(df$latitude)
    expect_error(floodCharacteristicWaterlevel(x = x, value = "MQ",
                               df = data.frame(gauging_station = "test",
                                               river = "Elbe",
                                               longitude = 1,
                                               latitude = "a")),
                 "inherits(df$latitude, \"numeric\") | inherits(df$latitude, ",
                 fixed = TRUE)
    
    # names(df)
    expect_error(floodCharacteristicWaterlevel(x = x, value = "MQ",
                               df = data.frame(gauging_station = "test",
                                               river = "Elbe",
                                               longitude = 1,
                                               latitude = 1)),
                 "\"km_csa\" %in% names(df) is not TRUE",
                 fixed = TRUE)
    # class(df$km_csa)
    expect_error(floodCharacteristicWaterlevel(x = x, value = "MQ",
                               df = data.frame(gauging_station = "test",
                                               river = "Elbe",
                                               longitude = 1,
                                               latitude = 1,
                                               km_csa = "1")),
                 "inherits(df$km_csa, \"numeric\") | inherits(df$km_csa, \"int",
                 fixed = TRUE)
    
    # names(df)
    expect_error(floodCharacteristicWaterlevel(x = x, value = "MQ",
                               df = data.frame(gauging_station = "test",
                                               river = "Elbe",
                                               longitude = 1,
                                               latitude = 1,
                                               km_csa = 1)),
                 "\"pnp\" %in% names(df) is not TRUE",
                 fixed = TRUE)
    # class(df$pnp)
    expect_error(floodCharacteristicWaterlevel(x = x, value = "MQ",
                               df = data.frame(gauging_station = "test",
                                               river = "Elbe",
                                               longitude = 1,
                                               latitude = 1,
                                               km_csa = 1,
                                               pnp = "0")),
                 "inherits(df$pnp, \"numeric\") | inherits(df$pnp, \"intege",
                 fixed = TRUE)
    
    # names(df)
    expect_error(floodCharacteristicWaterlevel(x = x, value = "MQ",
                               df = data.frame(gauging_station = "test",
                                               river = "Elbe",
                                               longitude = 1,
                                               latitude = 1,
                                               km_csa = 1,
                                               pnp = 0)),
                 "value %in% names(df) is not TRUE",
                 fixed = TRUE)
    # class(df[, value])
    expect_error(floodCharacteristicWaterlevel(x = x, value = "MQ",
                               df = data.frame(gauging_station = "test",
                                               river = "Elbe",
                                               longitude = 1,
                                               latitude = 1,
                                               km_csa = 1,
                                               pnp = 0,
                                               MQ = "0")),
                 "inherits(df[, value], \"numeric\") | inheri",
                 fixed = TRUE)
    
    # shift class
    expect_error(floodCharacteristicWaterlevel(x = x, value = "MQ",
                                               shift = "1"),
                 "inherits(shift, \"numeric\") | inhe")
    
    # shift length
    expect_error(floodCharacteristicWaterlevel(x = x, value = "MQ",
                                               shift = c(1, 2)),
                 "length(shift) == 1 is not TRUE",
                 fixed = TRUE)
    
    # filename class
    expect_error(floodCharacteristicWaterlevel(x = x, value = "MQ",
                                               filename = 1),
                 "'filename' must be type 'character'.")
    
    # filename length
    expect_error(floodCharacteristicWaterlevel(x = x, value = "MQ",
                                               filename = c("a", "b")), 
                 "'filename' must have length 1.")
})

test_that("floodCharacteristicWaterlevel: FLYS3", {
    if (Sys.info()["nodename"] == "pvil-r") {
        # Elbe
        hf <- Sys.getenv("hydflood")
        x <- hydSpatRaster(
            filename_dem = paste0(hf, "/data-raw/raster.dem.tif"),
            filename_csa = paste0(hf, "/data-raw/raster.csa.tif"))
        
        expect_no_condition(floodCharacteristicWaterlevel(x = x, value = "MQ"))
        
        # Rhine
        hf <- Sys.getenv("hydflood")
        x <- hydSpatRaster(
            filename_dem = paste0(hf, "/data-raw/raster.dem_plittersdorf.tif"),
            filename_csa = paste0(hf, "/data-raw/raster.csa_plittersdorf.tif"))
        
        expect_no_condition(floodCharacteristicWaterlevel(x = x, value = "MQ"))
    }
})

test_that("floodCharacteristicWaterlevel: Elbe_tidal", {
    if (Sys.info()["nodename"] == "pvil-r") {
        # Elbe_tidal
        p <- options()$hydflood.datadir
        df <- read.table("~/hydflood/data-raw/estuary/elbe/df_2010.csv",
                         header = TRUE, sep = ";", dec = ".")
        x <- hydSpatRaster(
            filename_dem = paste0(p, "/elbet01_GEESTHACHT_DEM.tif"),
            filename_csa = paste0(p, "/elbet01_GEESTHACHT_CSA.tif"))
        # fe <- floodCharacteristicWaterlevel(x = x, value = "MTnw")
        expect_no_condition(floodCharacteristicWaterlevel(x = x,
                                                          value = "MTnw",
                                                          df = df))
        
        x <- suppressWarnings(
            hydSpatRaster(
                filename_dem = paste0(p, "/elbet07_BROKDORF_DEM.tif"),
                filename_csa = paste0(p, "/elbet07_BROKDORF_CSA.tif"))
        )
        # fe <- floodCharacteristicWaterlevel(x = x, value = "MTnw")
        expect_no_condition(floodCharacteristicWaterlevel(x = x,
                                                          value = "MTnw",
                                                          df = df))
        
        x <- hydSpatRaster(
            filename_dem = paste0(p, "/elbet12_TRISCHEN_DEM.tif"),
            filename_csa = paste0(p, "/elbet12_TRISCHEN_CSA.tif"))
        expect_no_condition(floodCharacteristicWaterlevel(x = x,
                                                          value = "MTnw",
                                                          df = df))
    }
})

test_that("floodCharacteristicWaterlevel: Stoer_tidal", {
    if (Sys.info()["nodename"] == "pvil-r") {
        # Stoer_tidal
        p <- options()$hydflood.datadir
        x <- hydSpatRaster(
            filename_dem = paste0(p, "/elbet08_ITZEHOE_DEM.tif"),
            filename_csa = paste0(p, "/elbet08_ITZEHOE_CSA.tif"))
        expect_no_error(
            suppressWarnings(
                floodCharacteristicWaterlevel(x = x, value = "MTnw")))
    }
})

test_that("floodCharacteristicWaterlevel: Ems_tidal", {
    if (Sys.info()["nodename"] == "pvil-r") {
        
        # Ems_tidal
        p <- options()$hydflood.datadir
        df <- read.table("~/hydflood/data-raw/estuary/ems/df_2015.csv",
                         header = TRUE, sep = ";", dec = ".")
        
        # emst04_DOLLART
        x <- hydSpatRaster(
            filename_dem = paste0(p, "/emst04_DOLLART_DEM.tif"),
            filename_csa = paste0(p, "/emst04_DOLLART_CSA.tif"))
        expect_no_condition(floodCharacteristicWaterlevel(x = x,
                                                          value = "MTnw",
                                                          df = df))
        
        # emst15_SPIEKEROOG
        x <- hydSpatRaster(
            filename_dem = paste0(p, "/emst15_SPIEKEROOG_DEM.tif"),
            filename_csa = paste0(p, "/emst15_SPIEKEROOG_CSA.tif"))
        expect_no_condition(floodCharacteristicWaterlevel(x = x,
                                                          value = "MThw",
                                                          df = df))
    }
})

