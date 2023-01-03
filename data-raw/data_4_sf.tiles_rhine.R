
# store sf.tiles_rhine as external dataset
if (!(file.exists("data/sf.tiles_rhine.rda"))) {
    
    library("pangaear")
    
    # import shape
    sf.tiles_rhine <- st_read("data-raw/tiles_rhine.shp")
    names(sf.tiles_rhine) <- tolower(names(sf.tiles_rhine))
    
    # transformation into WGS 84
    sf.tiles_rhine_wgs84 <- st_transform(sf.tiles_rhine, wgs84)
    
    # add extents
    sf.tiles_rhine$xmin <- integer(nrow(sf.tiles_rhine))
    sf.tiles_rhine$xmax <- integer(nrow(sf.tiles_rhine))
    sf.tiles_rhine$ymin <- integer(nrow(sf.tiles_rhine))
    sf.tiles_rhine$ymax <- integer(nrow(sf.tiles_rhine))
    sf.tiles_rhine$lon_min <- numeric(nrow(sf.tiles_rhine))
    sf.tiles_rhine$lon_max <- numeric(nrow(sf.tiles_rhine))
    sf.tiles_rhine$lat_min <- numeric(nrow(sf.tiles_rhine))
    sf.tiles_rhine$lat_max <- numeric(nrow(sf.tiles_rhine))
    for (i in 1:nrow(sf.tiles_rhine)) {
        sf.tiles_rhine$xmin[i] <- as.integer(floor(st_bbox(sf.tiles_rhine[i,])$xmin))
        sf.tiles_rhine$xmax[i] <- as.integer(floor(st_bbox(sf.tiles_rhine[i,])$xmax))
        sf.tiles_rhine$ymin[i] <- as.integer(floor(st_bbox(sf.tiles_rhine[i,])$ymin))
        sf.tiles_rhine$ymax[i] <- as.integer(floor(st_bbox(sf.tiles_rhine[i,])$ymax))
        sf.tiles_rhine$lon_xmin[i] <- as.integer(floor(st_bbox(sf.tiles_rhine_wgs84[i,])$xmin))
        sf.tiles_rhine$lon_xmax[i] <- as.integer(floor(st_bbox(sf.tiles_rhine_wgs84[i,])$xmax))
        sf.tiles_rhine$lat_ymin[i] <- as.integer(floor(st_bbox(sf.tiles_rhine_wgs84[i,])$ymin))
        sf.tiles_rhine$lat_ymax[i] <- as.integer(floor(st_bbox(sf.tiles_rhine_wgs84[i,])$ymax))
    }
    
    # pangaea
    sf.tiles_rhine$url <- 
        as.data.frame(
            pangaear::pg_data(doi = "10.1594/PANGAEA.919308", overwrite = TRUE,
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
    cache_dem <- paste0("~/.hydflood/", sf.tiles_rhine$name, "_DEM.tif")
    
    write("# DEM cache files", stdout())
    for (i in 1:nrow(sf.tiles_rhine)) {
        if (! file.exists(cache_dem[i])){
            write(paste0(cache_dem[i], " will be downloaded"), stdout())
            download.file(sf.tiles_rhine$url[i], cache_dem[i], quiet = FALSE)
        } else {
            write(paste0(cache_dem[i], " exists already"), stdout())
        }
    }
    rm(i, cache_dem)
    write("", stdout())
    
    # reset crs to overcome R CHECK note of non-ascii characters
    st_crs(sf.tiles_rhine) <- ""
    
    # export
    usethis::use_data(sf.tiles_rhine, overwrite = TRUE,
                      compress = "bzip2")
    
    # clean up
    rm(sf.tiles_rhine_wgs84)
    
} else {
    write("data/sf.tiles_rhine.rda exists already", stderr())
}
