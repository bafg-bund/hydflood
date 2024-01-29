
# store sf.estuaries
if (!(file.exists("data/sf.estuaries.rda"))) {
    
    # compine areas into one single polygon dataset
    # Elbe_tidal
    x <- st_read("data-raw/estuary/elbe", "x", quiet = TRUE)
    x$id <- 1
    x$name <- "Elbe_tidal"
    sf.estuaries <- x[, c("geometry", "id", "name")]
    
    # Stoer
    x <- st_read("data-raw/estuary/stoer", "x", quiet = TRUE)
    x$id <- 2
    x$name <- "Stoer_tidal"
    sf.estuaries <- rbind(sf.estuaries, x[, c("geometry", "id", "name")])
    
    # Weser
    x <- st_read("data-raw/estuary/weser", "x", quiet = TRUE)
    x$id <- 3
    x$name <- "Weser_tidal"
    sf.estuaries <- rbind(sf.estuaries, x[, c("geometry", "id", "name")])
    
    # Jade
    x <- st_read("data-raw/estuary/jade", "x", quiet = TRUE)
    x$id <- 4
    x$name <- "Jade_tidal"
    sf.estuaries <- rbind(sf.estuaries, x[, c("geometry", "id", "name")])
    
    # Ems
    x <- st_read("data-raw/estuary/ems", "x", quiet = TRUE)
    x$id <- 5
    x$name <- "Ems_tidal"
    sf.estuaries <- rbind(sf.estuaries, x[, c("geometry", "id", "name")])
    
    # reset crs to overcome R CHECK note of non-ascii characters
    st_crs(sf.estuaries) <- ""
    
    # export
    usethis::use_data(sf.estuaries, overwrite = TRUE,
                      compress = "bzip2")
    
    # clean up
    rm(x, sf.estuaries)
    
} else {
    write("data/sf.estuaries.rda exists already", stderr())
}

# store sf.tiles_estuaries
if (!(file.exists("data/sf.tiles_estuaries.rda"))) {
    
    # compine areas into one single polygon dataset
    # Elbe_tidal
    x <- st_read("data-raw/estuary/elbe", "tiles_elbe", quiet = TRUE)
    x$id <- 1:nrow(x)
    x$river <- "Elbe_tidal"
    x$river[x$name == "elbet07_BROKDORF"] <- 
        paste0(x$river[x$name == "elbet07_BROKDORF"], ";Stoer_tidal")
    x$river[x$name == "elbet08_ITZEHOE"] <- "Stoer_tidal"
    sf.tiles_estuaries <- x[, c("geometry", "id", "name", "river", "url")]
    
    # Weser
    x <- st_read("data-raw/estuary/weser", "tiles_weser", quiet = TRUE)
    x$id <- 1:nrow(x) + max(sf.tiles_estuaries$id)
    x$river <- "Weser_tidal"
    x$river[which(x$name %in% 
                      c("wesert01_WUEMME", "wesert03_HUNTE"))] <- NA_character_
    sf.tiles_estuaries <- rbind(sf.tiles_estuaries,
                                x[, c("geometry", "id", "name", "river", "url")])
    
    # Jade
    x <- st_read("data-raw/estuary/jade", "tiles_jade", quiet = TRUE)
    x$id <- 1:nrow(x) + max(sf.tiles_estuaries$id)
    x$river <- "Jade_tidal"
    sf.tiles_estuaries <- rbind(sf.tiles_estuaries,
                                x[, c("geometry", "id", "name", "river", "url")])
    
    # Ems
    x <- st_read("data-raw/estuary/ems", "tiles_ems", quiet = TRUE)
    x$id <- 1:nrow(x) + max(sf.tiles_estuaries$id)
    x$river <- "Ems_tidal"
    x$river[which(x$name == "emst03_LEDA_JUEMME")] <- NA_character_
    sf.tiles_estuaries <- rbind(sf.tiles_estuaries,
                                x[, c("geometry", "id", "name", "river", "url")])
    
    # transformation into WGS 84
    sf.tiles_estuaries_wgs84 <- st_transform(sf.tiles_estuaries, wgs84)
    
    # add extents
    sf.tiles_estuaries$xmin <- integer(nrow(sf.tiles_estuaries))
    sf.tiles_estuaries$xmax <- integer(nrow(sf.tiles_estuaries))
    sf.tiles_estuaries$ymin <- integer(nrow(sf.tiles_estuaries))
    sf.tiles_estuaries$ymax <- integer(nrow(sf.tiles_estuaries))
    sf.tiles_estuaries$lon_xmin <- numeric(nrow(sf.tiles_estuaries))
    sf.tiles_estuaries$lon_xmax <- numeric(nrow(sf.tiles_estuaries))
    sf.tiles_estuaries$lat_ymin <- numeric(nrow(sf.tiles_estuaries))
    sf.tiles_estuaries$lat_ymax <- numeric(nrow(sf.tiles_estuaries))
    for (i in 1:nrow(sf.tiles_estuaries)) {
        sf.tiles_estuaries$xmin[i] <- 
            as.integer(floor(st_bbox(sf.tiles_estuaries[i,])$xmin))
        sf.tiles_estuaries$xmax[i] <- 
            as.integer(floor(st_bbox(sf.tiles_estuaries[i,])$xmax))
        sf.tiles_estuaries$ymin[i] <- 
            as.integer(floor(st_bbox(sf.tiles_estuaries[i,])$ymin))
        sf.tiles_estuaries$ymax[i] <- 
            as.integer(floor(st_bbox(sf.tiles_estuaries[i,])$ymax))
        sf.tiles_estuaries$lon_xmin[i] <- 
            st_bbox(sf.tiles_estuaries_wgs84[i,])$xmin
        sf.tiles_estuaries$lon_xmax[i] <- 
            st_bbox(sf.tiles_estuaries_wgs84[i,])$xmax
        sf.tiles_estuaries$lat_ymin[i] <- 
            st_bbox(sf.tiles_estuaries_wgs84[i,])$ymin
        sf.tiles_estuaries$lat_ymax[i] <- 
            st_bbox(sf.tiles_estuaries_wgs84[i,])$ymax
    }
    
    # pangaea
    # sf.tiles_estuaries$url <- ""
    #     as.data.frame(
    #         pangaear::pg_data(doi = "10.1594/PANGAEA.919293", overwrite = TRUE,
    #                           mssgs = FALSE)[[1]]$data)$`URL file`
    
    # reset crs to overcome R CHECK note of non-ascii characters
    st_crs(sf.tiles_estuaries) <- ""
    
    # export
    usethis::use_data(sf.tiles_estuaries, overwrite = TRUE,
                      compress = "bzip2")
    
    # clean up
    rm(x, sf.tiles_estuaries)
    
} else {
    write("data/sf.tiles_estuaries.rda exists already", stderr())
}
