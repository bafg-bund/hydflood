
# store spdf.tiles_elbe as external dataset
if (!(file.exists("data/spdf.tiles_elbe.rda"))) {
    
    library("rgdal")
    library("sp")
    library("pangaear")
    
    # import shape
    spdf.tiles_elbe <- rgdal::readOGR("data-raw", "tiles_elbe", FALSE,
                                      stringsAsFactors = FALSE)
    spdf.tiles_elbe$id <- as.integer(spdf.tiles_elbe$id)
    crs(spdf.tiles_elbe) <- UTM33N
    
    # transformation into WGS 84
    wgs84 <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    spdf.tiles_elbe_wgs84 <- spTransform(spdf.tiles_elbe, wgs84)
    
    # add extents
    spdf.tiles_elbe$xmin <- integer(nrow(spdf.tiles_elbe))
    spdf.tiles_elbe$xmax <- integer(nrow(spdf.tiles_elbe))
    spdf.tiles_elbe$ymin <- integer(nrow(spdf.tiles_elbe))
    spdf.tiles_elbe$ymax <- integer(nrow(spdf.tiles_elbe))
    spdf.tiles_elbe$lon_min <- numeric(nrow(spdf.tiles_elbe))
    spdf.tiles_elbe$lon_max <- numeric(nrow(spdf.tiles_elbe))
    spdf.tiles_elbe$lat_min <- numeric(nrow(spdf.tiles_elbe))
    spdf.tiles_elbe$lat_max <- numeric(nrow(spdf.tiles_elbe))
    for (i in 1:length(spdf.tiles_elbe)) {
        spdf.tiles_elbe$xmin[i] <- as.integer(floor(min(
            spdf.tiles_elbe@polygons[[i]]@Polygons[[1]]@coords[,1])))
        spdf.tiles_elbe$xmax[i] <- as.integer(ceiling(max(
            spdf.tiles_elbe@polygons[[i]]@Polygons[[1]]@coords[,1])))
        spdf.tiles_elbe$ymin[i] <- as.integer(floor(min(
            spdf.tiles_elbe@polygons[[i]]@Polygons[[1]]@coords[,2])))
        spdf.tiles_elbe$ymax[i] <- as.integer(ceiling(max(
            spdf.tiles_elbe@polygons[[i]]@Polygons[[1]]@coords[,2])))
        spdf.tiles_elbe$lon_min[i] <- min(
            spdf.tiles_elbe_wgs84@polygons[[i]]@Polygons[[1]]@coords[,1])
        spdf.tiles_elbe$lon_max[i] <- max(
            spdf.tiles_elbe_wgs84@polygons[[i]]@Polygons[[1]]@coords[,1])
        spdf.tiles_elbe$lat_min[i] <- min(
            spdf.tiles_elbe_wgs84@polygons[[i]]@Polygons[[1]]@coords[,2])
        spdf.tiles_elbe$lat_max[i] <- max(
            spdf.tiles_elbe_wgs84@polygons[[i]]@Polygons[[1]]@coords[,2])
    }
    
    # pangaea
    spdf.tiles_elbe$url <- 
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
    cache_dem <- paste0("~/.hydflood/", spdf.tiles_elbe$name, "_DEM.tif")
    
    write("# DEM cache files", stdout())
    for (i in 1:length(spdf.tiles_elbe)) {
        if (! file.exists(cache_dem[i])){
            write(paste0(cache_dem[i], " will be downloaded"), stdout())
            download.file(spdf.tiles_elbe$url[i], cache_dem[i], quiet = FALSE)
        } else {
            write(paste0(cache_dem[i], " exists already"), stdout())
        }
    }
    rm(i, cache_dem)
    write("", stdout())
    
    # export
    usethis::use_data(spdf.tiles_elbe, overwrite = TRUE,
                      compress = "bzip2")
    
    # clean up
    rm(spdf.tiles_elbe_wgs84, wgs84)
    
} else {
    write("data/spdf.tiles_elbe.rda exists already", stderr())
}
