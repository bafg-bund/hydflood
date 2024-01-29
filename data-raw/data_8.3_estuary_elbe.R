
# store sf.estuary_elbe_sections as external dataset
if (!(file.exists("data-raw/estuary/elbe/sf.estuary_elbe_sections.rda"))) {
    s <- st_read(paste0("~/freigaben/Produkte/Befliegungen_M53/_Elbe/2016_Belf",
                        "-TA-Elbe_2015-17_km585-752/ETRS89_GRS80_UTM32_DHHN92/",
                        "10_Zusatz/Abschnitte"), "Befl_TA_Elbe2016_Abschnitte")
    names(s) <- tolower(names(s))
    s$fläche <- NULL
    names(s)[1] <- "name"
    s <- s[-which(s$name %in% c("Oste", "Hemmoor", "Oberndorf")), ]
    
    # put them in a specific order (upstream => downstream)
    row.names(s) <- s$name
    s <- s[c("Geesthacht", "Hamburg", "Buxtehude", "Wedel", "Pinnau",
             "Krueckau", "Brokdorf", "Itzehoe", "Brunsbuettel", "Otterndorf",
             "Cuxhaven", "Trischen", "Neuwerk", "Vogelsand", "Hohenhoern",
             "Scharhoern"), ]
    s$id <- 1:nrow(s)
    row.names(s) <- s$id
    
    # transformation into WGS 84
    s_wgs84 <- st_transform(s, wgs84)
    
    # add extents
    s$xmin <- integer(nrow(s))
    s$xmax <- integer(nrow(s))
    s$ymin <- integer(nrow(s))
    s$ymax <- integer(nrow(s))
    s$lon_xmin <- numeric(nrow(s))
    s$lon_xmax <- numeric(nrow(s))
    s$lat_ymin <- numeric(nrow(s))
    s$lat_ymax <- numeric(nrow(s))
    for (i in 1:nrow(s)) {
        s$xmin[i] <- as.integer(floor(st_bbox(s[i,])$xmin))
        s$xmax[i] <- as.integer(floor(st_bbox(s[i,])$xmax))
        s$ymin[i] <- as.integer(floor(st_bbox(s[i,])$ymin))
        s$ymax[i] <- as.integer(floor(st_bbox(s[i,])$ymax))
        s$lon_xmin[i] <- st_bbox(s_wgs84[i,])$xmin
        s$lon_xmax[i] <- st_bbox(s_wgs84[i,])$xmax
        s$lat_ymin[i] <- st_bbox(s_wgs84[i,])$ymin
        s$lat_ymax[i] <- st_bbox(s_wgs84[i,])$ymax
    }
    s$url <- paste0("http://r.bafg.de/~WeberA/dgms/data/elbet",
                    sprintf("%02.0f", s$id), "_", toupper(s$name), "_DEM.tif")
    s$name <- paste0("elbet", sprintf("%02.0f", s$id), "_", toupper(s$name))
    
    sf.estuary_elbe_sections <- s[, c("id", "name", "xmin", "xmax", "ymin",
                                      "ymax", "lon_xmin", "lon_xmax",
                                      "lat_ymin", "lat_ymax", "url",
                                      "geometry")]
    save(sf.estuary_elbe_sections, compress = "bzip2", 
         file = "data-raw/estuary/elbe/sf.estuary_elbe_sections.rda")
    st_write(sf.estuary_elbe_sections, "data-raw/estuary/elbe", "tiles_elbe",
             driver = "ESRI Shapefile", append = FALSE)
    
} else {
    write("data-raw/estuary/elbe/sf.estuary_elbe_sections.rda exists already", 
          stderr())
}

# store sf.estuary_elbe_csa as external dataset
if (!(file.exists("data-raw/estuary/elbe/sf.estuary_elbe_csa.rda"))) {
    # import the required geometries and other data
    # area
    x <- st_read("data-raw/estuary/elbe", "x", quiet = TRUE)
    
    # axis
    axis <- st_read("data-raw/estuary/elbe", "axis", quiet = TRUE)
    
    # left
    left <- st_read("data-raw/estuary/elbe", "left", quiet = TRUE)
    
    # right
    right <- st_read("data-raw/estuary/elbe", "right", quiet = TRUE)
    
    # gs
    g <- df.gauging_station_data[df.gauging_station_data$data_present &
                                 df.gauging_station_data$river == "ELBE_tidal",]
    gs <- st_as_sf(g, coords = c("longitude", "latitude"), remove = FALSE)
    st_crs(gs) <- st_crs("EPSG:4326")
    gs <- st_transform(gs, st_crs("EPSG:25832"))
    gs <- gs[x, ]
    # st_write(gs, "data-raw/estuary/elbe", "gs", driver = "ESRI Shapefile")
    # gs <- st_read("data-raw/estuary/elbe", "gs", quiet = TRUE)
    
    # compute csa
    sf.estuary_elbe_csa_lines <- createEstuaryCSA(x, axis, left, right, 1/1000,
                                                  gs, "lines")
    save(sf.estuary_elbe_csa_lines, compress = "bzip2", 
         file = "data-raw/estuary/elbe/sf.estuary_elbe_csa_lines.rda")
    st_write(sf.estuary_elbe_csa_lines, "data-raw/estuary/elbe",
             "sf.estuary_elbe_csa_lines", driver = "ESRI Shapefile",
             append = FALSE)
    
    
    sf.estuary_elbe_csa <- sf.estuary_elbe_csa_lines
    save(sf.estuary_elbe_csa, compress = "bzip2", 
         file = "data-raw/estuary/elbe/sf.estuary_elbe_csa.rda")
    if (!file.exists("docs/downloads/sf.estuary_elbe_csa.rda")) {
        save(sf.estuary_elbe_csa, compress = "bzip2", 
             file = "docs/downloads/sf.estuary_elbe_csa.rda")
    }
    if (!is.null(options()$hydflood.datadir)) {
        f <- paste0(options()$hydflood.datadir, "/sf.estuary_elbe_csa.rda")
        if (!file.exists(f)) {
            save(sf.estuary_elbe_csa, compress = "bzip2", file = f)
        }
    }
    
    # sf.estuary_elbe_csa_linesplit <- 
    #     createEstuaryCSA(x, axis, left, right, 1/1000, gs, "linesplit")
    # save(sf.estuary_elbe_csa_linesplit, compress = "bzip2", 
    #      file = "data-raw/estuary/elbe/sf.estuary_elbe_csa_linesplit.rda")
    # st_write(sf.estuary_elbe_csa_linesplit, "data-raw/estuary/elbe",
    #          "sf.estuary_elbe_csa_linesplit", driver = "ESRI Shapefile",
    #          append = FALSE)
    # 
    # sf.estuary_elbe_csa_default <- 
    #     createEstuaryCSA(x, axis, left, right, 1/1000, gs, "default")
    # save(sf.estuary_elbe_csa_default, compress = "bzip2", 
    #      file = "data-raw/estuary/elbe/sf.estuary_elbe_csa_default.rda")
    # st_write(sf.estuary_elbe_csa_default, "data-raw/estuary/elbe",
    #          "sf.estuary_elbe_csa_default", driver = "ESRI Shapefile",
    #          append = FALSE)
    
    # # update df.gauging_station_data of hyd1d
    if (require(RPostgreSQL) & require(DBI)) {
        # connect to DB
        credentials <- hyd1d:::credentials("~/hyd1d/DB_credentials_gauging_data")
        con <- dbConnect("PostgreSQL",
                         host = credentials["host"],
                         dbname = credentials["dbname"],
                         user = credentials["user"],
                         password = credentials["password"],
                         port = credentials["port"])
        postgresqlpqExec(con, "SET client_encoding = 'UTF-8'")
        
        # query "new" stationing spatially
        id <- unlist(st_covered_by(gs, sf.estuary_elbe_csa))
        gs$csa <- sf.estuary_elbe_csa[id, ]$station
        
        # update km_qps
        for (i in 1:nrow(gs)) {
            dbExecute(con, paste0("UPDATE gauging_station_data SET km_qps = ",
                                  gs$csa[i],
                                  " WHERE water_longname = \'", gs$river[i],
                                  "\' AND gauging_station = \'",
                                  gs$gauging_station[i], "\'"))
        }
        dbDisconnect(con)
    }
    
} else {
    write("data-raw/estuary/elbe/sf.estuary_elbe_csa.rda exists already", 
          stderr())
    load("data-raw/estuary/elbe/sf.estuary_elbe_csa.rda")
}

# convert sf.estuary_elbe_csa to rasters
if (!is.null(options()$hydflood.datadir)) {
    if (dir.exists(options()$hydflood.datadir)) {
        load("data-raw/estuary/elbe/sf.estuary_elbe_sections.rda")
        files <- paste0(options()$hydflood.datadir, "/",
                        sf.estuary_elbe_sections$name, "_CSA.tif")
        
        for (a_file in files) {
            
            # get the existing dem
            dem <- rast(gsub("CSA", "DEM", a_file))
            
            if (file.exists(a_file)) {
                print(paste0(a_file, " exists already"))
            } else {
                print(paste0(a_file, " is produced"))
                
                # create an empty raster
                csa <- rast(x = ext(dem), resolution = 1, vals = NA,
                            crs = crs("EPSG:25832"))
                
                # convert sf.estuary_elbe_csa to
                csa <- rasterize(vect(sf.estuary_elbe_csa), csa,
                                 field = "station_int", update = TRUE)
                
                writeRaster(csa, datatype = "INT4S", filename = a_file,
                            gdal = c("COMPRESS=LZW", "TFW=NO"),
                            overwrite = TRUE)
            }
            
            csa <- rast(a_file)
            if (!ext(dem) == ext(csa)) {
                print(ext(dem))
                print(ext(csa))
            }
        }
    }
}

rm(sf.estuary_elbe_csa)

# store sf.estuary_elbe_tiles as external dataset
# if (!(file.exists("data-raw/estuary/elbe/sf.estuary_elbe_tiles.rda"))) {
#     # area
#     x <- st_read("data-raw/estuary/elbe", "x", quiet = TRUE)
#     sf.estuary_elbe_tiles <- createTiles(x, 10000, 10000)
#     
#     save(sf.estuary_elbe_tiles, compress = "bzip2", 
#          file = "data-raw/estuary/elbe/sf.estuary_elbe_tiles.rda")
# } else {
#     write("data-raw/estuary/elbe/sf.estuary_elbe_tiles.rda exists already", 
#           stderr())
# }
