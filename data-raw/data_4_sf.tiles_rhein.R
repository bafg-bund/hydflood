
# store sf.tiles_rhein as external dataset
if (!(file.exists("data/sf.tiles_rhein.rda"))) {
    
    library("pangaear")
    
    # import shape
    sf.tiles_rhein <- st_read("data-raw/tiles_rhein.shp")
    
    # transformation into WGS 84
    sf.tiles_rhein_wgs84 <- st_transform(sf.tiles_rhein, wgs84)
    
    # add extents
    sf.tiles_rhein$xmin <- integer(nrow(sf.tiles_rhein))
    sf.tiles_rhein$xmax <- integer(nrow(sf.tiles_rhein))
    sf.tiles_rhein$ymin <- integer(nrow(sf.tiles_rhein))
    sf.tiles_rhein$ymax <- integer(nrow(sf.tiles_rhein))
    sf.tiles_rhein$lon_min <- numeric(nrow(sf.tiles_rhein))
    sf.tiles_rhein$lon_max <- numeric(nrow(sf.tiles_rhein))
    sf.tiles_rhein$lat_min <- numeric(nrow(sf.tiles_rhein))
    sf.tiles_rhein$lat_max <- numeric(nrow(sf.tiles_rhein))
    for (i in 1:nrow(sf.tiles_rhein)) {
        sf.tiles_rhein$xmin[i] <- as.integer(floor(st_bbox(sf.tiles_rhein[i,])$xmin))
        sf.tiles_rhein$xmax[i] <- as.integer(floor(st_bbox(sf.tiles_rhein[i,])$xmax))
        sf.tiles_rhein$ymin[i] <- as.integer(floor(st_bbox(sf.tiles_rhein[i,])$ymin))
        sf.tiles_rhein$ymax[i] <- as.integer(floor(st_bbox(sf.tiles_rhein[i,])$ymax))
        sf.tiles_rhein$lon_xmin[i] <- as.integer(floor(st_bbox(sf.tiles_rhein_wgs84[i,])$xmin))
        sf.tiles_rhein$lon_xmax[i] <- as.integer(floor(st_bbox(sf.tiles_rhein_wgs84[i,])$xmax))
        sf.tiles_rhein$lat_ymin[i] <- as.integer(floor(st_bbox(sf.tiles_rhein_wgs84[i,])$ymin))
        sf.tiles_rhein$lat_ymax[i] <- as.integer(floor(st_bbox(sf.tiles_rhein_wgs84[i,])$ymax))
    }
    
    # pangaea
    sf.tiles_rhein$url <- 
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
    cache_dem <- paste0("~/.hydflood/", sf.tiles_rhein$name, "_DEM.tif")
    
    write("# DEM cache files", stdout())
    for (i in 1:nrow(sf.tiles_rhein)) {
        if (! file.exists(cache_dem[i])){
            write(paste0(cache_dem[i], " will be downloaded"), stdout())
            download.file(sf.tiles_rhein$url[i], cache_dem[i], quiet = FALSE)
        } else {
            write(paste0(cache_dem[i], " exists already"), stdout())
        }
    }
    rm(i, cache_dem)
    write("", stdout())
    
    # reset crs to overcome R CHECK note of non-ascii characters
    st_crs(sf.tiles_rhein) <- ""
    
    # export
    usethis::use_data(sf.tiles_rhein, overwrite = TRUE,
                      compress = "bzip2")
    
    # clean up
    rm(sf.tiles_rhein_wgs84)
    
} else {
    write("data/sf.tiles_rhein.rda exists already", stderr())
}
