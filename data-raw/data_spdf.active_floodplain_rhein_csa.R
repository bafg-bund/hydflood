
# store spdf.active_floodplain_rhein_csa as external dataset
if (!(file.exists("data-raw/spdf.active_floodplain_rhein_csa.rda"))) {
    
    library("rgdal")
    library("sp")
    library("sf")
    
    # import
    spdf.active_floodplain_rhein_csa <- readOGR(dsn = "data-raw",
        layer = "active_floodplain_rhein_csa", verbose = FALSE,
        p4s = "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")
    spdf.active_floodplain_rhein_csa$STATION_INT <- 
        as.integer(round(spdf.active_floodplain_rhein_csa$STATION * 1000))
    spdf.active_floodplain_rhein_csa$OBJECTID <- NULL
    spdf.active_floodplain_rhein_csa$STATION_IN <- NULL
    # spdf.active_floodplain_rhein_csa$SECTION_DO <- NULL
    # spdf.active_floodplain_rhein_csa$SECTION_KM <- NULL
    # spdf.active_floodplain_rhein_csa$FROM_KM <- NULL
    # spdf.active_floodplain_rhein_csa$TO_KM <- NULL
    # spdf.active_floodplain_rhein_csa$GS_UP <- NULL
    # spdf.active_floodplain_rhein_csa$GS_DO <- NULL
    
    if (!"SECTION" %in% names(spdf.active_floodplain_rhein_csa)) {
        spdf.tiles <- readOGR(dsn = "~/flut3_Rhein/data/shp",
                              layer = "tiles", verbose = FALSE)
        spdf.tiles <- spdf.tiles[,c("name")]
        names(spdf.tiles) <- "SECTION"
        sf.af <- st_as_sf(spdf.active_floodplain_rhein_csa)
        sf.t <- st_as_sf(spdf.tiles)
        sf.aft <- st_join(sf.af, sf.t)
        sf.aft <- sf.aft[!grepl(".", row.names(sf.aft), fixed = TRUE), ]
        spdf.active_floodplain_rhein_csa <- as(sf.aft, "Spatial")
        rm(spdf.tiles, sf.af, sf.t, sf.aft)
    }
    
    spdf.active_floodplain_rhein_csa$SECTION <- as.character(spdf.active_floodplain_rhein_csa$SECTION)
    proj4string(spdf.active_floodplain_rhein_csa) <- 
        CRS("+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")
    
    if (!"SECTION_DO" %in% names(spdf.active_floodplain_rhein_csa)) {
        
        spdf.active_floodplain_rhein_csa$SECTION_DO <- 
            rep(NA_character_, nrow(spdf.active_floodplain_rhein_csa))
        
        spdf.tiles <- readOGR(dsn = "~/flut3_Rhein/data/shp",
                              layer = "tiles", verbose = FALSE,
                              p4s = "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")
        spdf.tiles <- spdf.tiles[,c("NAME")]
        names(spdf.tiles) <- "SECTION"
        
        for (i in 1:(nrow(spdf.tiles) - 1)) {
            # in section
            section <- spdf.tiles$SECTION[i]
            id_sec <- row.names(spdf.active_floodplain_rhein_csa[
                which(spdf.active_floodplain_rhein_csa$SECTION == section), ])
            
            # in downstream tile
            section_do <- spdf.tiles$SECTION[i + 1]
            spdf.tile_sel <- spdf.tiles[i + 1,]
            id_sel <- row.names(spdf.active_floodplain_rhein_csa[spdf.tile_sel,])
            
            id <- id_sec[id_sec %in% id_sel]
            spdf.active_floodplain_rhein_csa[id, "SECTION_DO"] <- as.character(section_do)
        }
    }
    
    # reorder columns
    spdf.active_floodplain_rhein_csa <- spdf.active_floodplain_rhein_csa[, c(
        "STATION", "STATION_INT", "SECTION", "SECTION_DO", "SECTION_KM",
        "FROM_KM", "TO_KM", "GS_UP", "GS_DO")]
    
    # export
    proj4string(spdf.active_floodplain_rhein_csa) <- 
        CRS("+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")
    usethis::use_data(spdf.active_floodplain_rhein_csa,
                      overwrite = TRUE, compress = "bzip2")
    system("mv data/spdf.active_floodplain_rhein_csa.rda data-raw/")
    system("cp data-raw/spdf.active_floodplain_rhein_csa.rda ~/.hydflood/")
    
    # clean up
    rm(spdf.active_floodplain_rhein_csa)
    
} else {
    write("data-raw/spdf.active_floodplain_rhein_csa.rda exists already",
          stderr())
}

