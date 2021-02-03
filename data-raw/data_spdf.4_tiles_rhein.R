
# store spdf.tiles_rhein as external dataset
if (!(file.exists("data/spdf.tiles_rhein.rda"))) {
    
    library("rgdal")
    library("sp")
    library("pangaear")
    
    # import shape
    spdf.tiles_rhein <- rgdal::readOGR("data-raw", "tiles_rhein", FALSE,
                                      stringsAsFactors = FALSE)
    spdf.tiles_rhein$id <- as.integer(spdf.tiles_rhein$id)
    crs(spdf.tiles_rhein) <- UTM32N
    
    # transformation into WGS 84
    wgs84 <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    spdf.tiles_rhein_wgs84 <- spTransform(spdf.tiles_rhein, wgs84)
    
    # add extents
    spdf.tiles_rhein$xmin <- integer(nrow(spdf.tiles_rhein))
    spdf.tiles_rhein$xmax <- integer(nrow(spdf.tiles_rhein))
    spdf.tiles_rhein$ymin <- integer(nrow(spdf.tiles_rhein))
    spdf.tiles_rhein$ymax <- integer(nrow(spdf.tiles_rhein))
    spdf.tiles_rhein$lon_min <- numeric(nrow(spdf.tiles_rhein))
    spdf.tiles_rhein$lon_max <- numeric(nrow(spdf.tiles_rhein))
    spdf.tiles_rhein$lat_min <- numeric(nrow(spdf.tiles_rhein))
    spdf.tiles_rhein$lat_max <- numeric(nrow(spdf.tiles_rhein))
    for (i in 1:length(spdf.tiles_rhein)) {
        spdf.tiles_rhein$xmin[i] <- as.integer(floor(min(
            spdf.tiles_rhein@polygons[[i]]@Polygons[[1]]@coords[,1])))
        spdf.tiles_rhein$xmax[i] <- as.integer(ceiling(max(
            spdf.tiles_rhein@polygons[[i]]@Polygons[[1]]@coords[,1])))
        spdf.tiles_rhein$ymin[i] <- as.integer(floor(min(
            spdf.tiles_rhein@polygons[[i]]@Polygons[[1]]@coords[,2])))
        spdf.tiles_rhein$ymax[i] <- as.integer(ceiling(max(
            spdf.tiles_rhein@polygons[[i]]@Polygons[[1]]@coords[,2])))
        spdf.tiles_rhein$lon_min[i] <- min(
            spdf.tiles_rhein_wgs84@polygons[[i]]@Polygons[[1]]@coords[,1])
        spdf.tiles_rhein$lon_max[i] <- max(
            spdf.tiles_rhein_wgs84@polygons[[i]]@Polygons[[1]]@coords[,1])
        spdf.tiles_rhein$lat_min[i] <- min(
            spdf.tiles_rhein_wgs84@polygons[[i]]@Polygons[[1]]@coords[,2])
        spdf.tiles_rhein$lat_max[i] <- max(
            spdf.tiles_rhein_wgs84@polygons[[i]]@Polygons[[1]]@coords[,2])
    }
    
    # pangaea
    spdf.tiles_rhein$url <- 
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
    cache_dem <- paste0("~/.hydflood/", spdf.tiles_rhein$name, "_DEM.tif")
    
    write("# DEM cache files", stdout())
    for (i in 1:length(spdf.tiles_rhein)) {
        if (! file.exists(cache_dem[i])){
            write(paste0(cache_dem[i], " will be downloaded"), stdout())
            download.file(spdf.tiles_rhein$url[i], cache_dem[i], quiet = FALSE)
        } else {
            write(paste0(cache_dem[i], " exists already"), stdout())
        }
    }
    rm(i, cache_dem)
    write("", stdout())
    
    # export
    usethis::use_data(spdf.tiles_rhein, overwrite = TRUE,
                      compress = "bzip2")
    
    # clean up
    rm(spdf.tiles_rhein_wgs84, wgs84)
    
} else {
    write("data/spdf.tiles_rhein.rda exists already", stderr())
}
