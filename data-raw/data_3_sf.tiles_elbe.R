
# store sf.tiles_elbe as external dataset
if (!(file.exists("data/sf.tiles_elbe.rda"))) {
    
    library("pangaear")
    
    # import shape
    sf.tiles_elbe <- st_read("data-raw/tiles_elbe.shp")
    names(sf.tiles_elbe) <- tolower(names(sf.tiles_elbe))
    
    # transformation into WGS 84
    sf.tiles_elbe_wgs84 <- st_transform(sf.tiles_elbe, wgs84)
    
    # add extents
    sf.tiles_elbe$xmin <- integer(nrow(sf.tiles_elbe))
    sf.tiles_elbe$xmax <- integer(nrow(sf.tiles_elbe))
    sf.tiles_elbe$ymin <- integer(nrow(sf.tiles_elbe))
    sf.tiles_elbe$ymax <- integer(nrow(sf.tiles_elbe))
    sf.tiles_elbe$lon_xmin <- numeric(nrow(sf.tiles_elbe))
    sf.tiles_elbe$lon_xmax <- numeric(nrow(sf.tiles_elbe))
    sf.tiles_elbe$lat_ymin <- numeric(nrow(sf.tiles_elbe))
    sf.tiles_elbe$lat_ymax <- numeric(nrow(sf.tiles_elbe))
    for (i in 1:nrow(sf.tiles_elbe)) {
        sf.tiles_elbe$xmin[i] <- as.integer(floor(st_bbox(sf.tiles_elbe[i,])$xmin))
        sf.tiles_elbe$xmax[i] <- as.integer(floor(st_bbox(sf.tiles_elbe[i,])$xmax))
        sf.tiles_elbe$ymin[i] <- as.integer(floor(st_bbox(sf.tiles_elbe[i,])$ymin))
        sf.tiles_elbe$ymax[i] <- as.integer(floor(st_bbox(sf.tiles_elbe[i,])$ymax))
        sf.tiles_elbe$lon_xmin[i] <- st_bbox(sf.tiles_elbe_wgs84[i,])$xmin
        sf.tiles_elbe$lon_xmax[i] <- st_bbox(sf.tiles_elbe_wgs84[i,])$xmax
        sf.tiles_elbe$lat_ymin[i] <- st_bbox(sf.tiles_elbe_wgs84[i,])$ymin
        sf.tiles_elbe$lat_ymax[i] <- st_bbox(sf.tiles_elbe_wgs84[i,])$ymax
    }
    
    # pangaea
    sf.tiles_elbe$url <- 
        as.data.frame(
            pangaear::pg_data(doi = "10.1594/PANGAEA.919293", overwrite = TRUE,
                              mssgs = FALSE)[[1]]$data)$`URL file`
    
    # create and fill cache
    # directory
    if (! dir.exists("~/.hydflood")) {
        write("# cache directory", stdout())
        write("~/.hydflood was created for DEM caching", stdout())
        dir.create("~/.hydflood", FALSE, TRUE)
        write("", stdout())
    }
    
    # files
    cache_dem <- paste0("~/.hydflood/", sf.tiles_elbe$name, "_DEM.tif")
    
    write("# DEM cache files", stdout())
    for (i in 1:nrow(sf.tiles_elbe)) {
        if (! file.exists(cache_dem[i])){
            write(paste0(cache_dem[i], " will be downloaded"), stdout())
            download.file(sf.tiles_elbe$url[i], cache_dem[i], quiet = FALSE)
        } else {
            write(paste0(cache_dem[i], " exists already"), stdout())
        }
    }
    rm(i, cache_dem)
    write("", stdout())
    
    # reset crs to overcome R CHECK note of non-ascii characters
    st_crs(sf.tiles_elbe) <- ""
    
    # export
    usethis::use_data(sf.tiles_elbe, overwrite = TRUE,
                      compress = "bzip2")
    
    # clean up
    rm(sf.tiles_elbe_wgs84)
    
} else {
    write("data/sf.tiles_elbe.rda exists already", stderr())
}

