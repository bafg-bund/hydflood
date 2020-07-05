

# store spdf.tiles_elbe as external dataset
if (!(file.exists("data/spdf.tiles_elbe.rda"))) {
    
    library("rgdal")
    library("sp")
    library("pangaear")
    
    # import shape
    spdf.tiles_elbe <- rgdal::readOGR("data-raw", "tiles_elbe", FALSE,
                                      stringsAsFactors = FALSE,
                                      encoding = "UTF-8", use_iconv = TRUE)
    names(spdf.tiles_elbe) <- c("id", "name")
    spdf.tiles_elbe$id <- as.integer(spdf.tiles_elbe$id)
    
    # transformation into WGS 84
    wgs84 <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    spdf.tiles_elbe_wgs84 <- spTransform(spdf.tiles_elbe, wgs84)
    
    # add extents
    spdf.tiles_elbe$xmin <- numeric(nrow(spdf.tiles_elbe))
    spdf.tiles_elbe$xmax <- numeric(nrow(spdf.tiles_elbe))
    spdf.tiles_elbe$ymin <- numeric(nrow(spdf.tiles_elbe))
    spdf.tiles_elbe$ymax <- numeric(nrow(spdf.tiles_elbe))
    spdf.tiles_elbe$lon_min <- numeric(nrow(spdf.tiles_elbe))
    spdf.tiles_elbe$lon_max <- numeric(nrow(spdf.tiles_elbe))
    spdf.tiles_elbe$lat_min <- numeric(nrow(spdf.tiles_elbe))
    spdf.tiles_elbe$lat_max <- numeric(nrow(spdf.tiles_elbe))
    for (i in 1:length(spdf.tiles_elbe)) {
        spdf.tiles_elbe$xmin[i] <- min(spdf.tiles_elbe@polygons[[i]]@Polygons[[1]]@coords[,1])
        spdf.tiles_elbe$xmax[i] <- max(spdf.tiles_elbe@polygons[[i]]@Polygons[[1]]@coords[,1])
        spdf.tiles_elbe$ymin[i] <- min(spdf.tiles_elbe@polygons[[i]]@Polygons[[1]]@coords[,2])
        spdf.tiles_elbe$ymax[i] <- max(spdf.tiles_elbe@polygons[[i]]@Polygons[[1]]@coords[,2])
        spdf.tiles_elbe$lon_min[i] <- min(spdf.tiles_elbe_wgs84@polygons[[i]]@Polygons[[1]]@coords[,1])
        spdf.tiles_elbe$lon_max[i] <- max(spdf.tiles_elbe_wgs84@polygons[[i]]@Polygons[[1]]@coords[,1])
        spdf.tiles_elbe$lat_min[i] <- min(spdf.tiles_elbe_wgs84@polygons[[i]]@Polygons[[1]]@coords[,2])
        spdf.tiles_elbe$lat_max[i] <- max(spdf.tiles_elbe_wgs84@polygons[[i]]@Polygons[[1]]@coords[,2])
    }
    
    # pangaea
    spdf.tiles_elbe$url <- 
        as.data.frame(
            pangaear::pg_data(doi = "10.1594/PANGAEA.919293", overwrite = TRUE,
                              mssgs = FALSE)[[1]]$data)$`URL file`
    
    # hard set of CRS
    crs(spdf.tiles_elbe) <- sp::CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")
    
    # export
    usethis::use_data(spdf.tiles_elbe, overwrite = TRUE,
                      compress = "bzip2")
    
    # clean up
    rm(spdf.tiles_elbe, spdf.tiles_elbe_wgs84)
    
} else {
    write("data/spdf.tiles_elbe.rda exists already", stderr())
}
