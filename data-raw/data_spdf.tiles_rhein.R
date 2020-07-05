

# store spdf.tiles_rhein as external dataset
if (!(file.exists("data/spdf.tiles_rhein.rda"))) {
    
    library("rgdal")
    library("sp")
    library("pangaear")
    
    # import shape
    spdf.tiles_rhein <- rgdal::readOGR("data-raw", "tiles_rhein", FALSE,
                                      stringsAsFactors = FALSE,
                                      encoding = "UTF-8", use_iconv = TRUE)
    names(spdf.tiles_rhein) <- c("id", "name")
    
    # transformation into WGS 84
    wgs84 <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    spdf.tiles_rhein_wgs84 <- spTransform(spdf.tiles_rhein, wgs84)
    
    # add extents
    spdf.tiles_rhein$xmin <- numeric(nrow(spdf.tiles_rhein))
    spdf.tiles_rhein$xmax <- numeric(nrow(spdf.tiles_rhein))
    spdf.tiles_rhein$ymin <- numeric(nrow(spdf.tiles_rhein))
    spdf.tiles_rhein$ymax <- numeric(nrow(spdf.tiles_rhein))
    spdf.tiles_rhein$lon_min <- numeric(nrow(spdf.tiles_rhein))
    spdf.tiles_rhein$lon_max <- numeric(nrow(spdf.tiles_rhein))
    spdf.tiles_rhein$lat_min <- numeric(nrow(spdf.tiles_rhein))
    spdf.tiles_rhein$lat_max <- numeric(nrow(spdf.tiles_rhein))
    for (i in 1:length(spdf.tiles_rhein)) {
        spdf.tiles_rhein$xmin[i] <- min(spdf.tiles_rhein@polygons[[i]]@Polygons[[1]]@coords[,1])
        spdf.tiles_rhein$xmax[i] <- max(spdf.tiles_rhein@polygons[[i]]@Polygons[[1]]@coords[,1])
        spdf.tiles_rhein$ymin[i] <- min(spdf.tiles_rhein@polygons[[i]]@Polygons[[1]]@coords[,2])
        spdf.tiles_rhein$ymax[i] <- max(spdf.tiles_rhein@polygons[[i]]@Polygons[[1]]@coords[,2])
        spdf.tiles_rhein$lon_min[i] <- min(spdf.tiles_rhein_wgs84@polygons[[i]]@Polygons[[1]]@coords[,1])
        spdf.tiles_rhein$lon_max[i] <- max(spdf.tiles_rhein_wgs84@polygons[[i]]@Polygons[[1]]@coords[,1])
        spdf.tiles_rhein$lat_min[i] <- min(spdf.tiles_rhein_wgs84@polygons[[i]]@Polygons[[1]]@coords[,2])
        spdf.tiles_rhein$lat_max[i] <- max(spdf.tiles_rhein_wgs84@polygons[[i]]@Polygons[[1]]@coords[,2])
    }
    
    # pangaea
    spdf.tiles_rhein$url <- 
        as.data.frame(
            pangaear::pg_data(doi = "10.1594/PANGAEA.919308", overwrite = TRUE,
                              mssgs = FALSE)[[1]]$data)$`URL file`
    
    # hard set of CRS
    crs(spdf.tiles_rhein) <- sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")
    
    # export
    usethis::use_data(spdf.tiles_rhein, overwrite = TRUE,
                      compress = "bzip2")
    
    # clean up
    rm(spdf.tiles_rhein, spdf.tiles_rhein_wgs84)
    
} else {
    write("data/spdf.tiles_rhein.rda exists already", stderr())
}
