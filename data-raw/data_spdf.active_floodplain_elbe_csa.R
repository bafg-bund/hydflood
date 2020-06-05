
# store spdf.active_floodplain_elbe_csa as external dataset
if (!(file.exists("data-raw/spdf.active_floodplain_elbe_csa.rda"))) {
    
    library("rgdal")
    library("sp")
    library("sf")
    
    # import
    spdf.active_floodplain_elbe_csa <- readOGR(dsn = "data-raw",
        layer = "active_floodplain_elbe_csa", verbose = FALSE,
        p4s = "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")
    spdf.active_floodplain_elbe_csa$STATION_INT <- 
        as.integer(round(spdf.active_floodplain_elbe_csa$STATION * 10) * 100)
    spdf.active_floodplain_elbe_csa$STATION_IN <- NULL
    spdf.active_floodplain_elbe_csa$Shape_Area <- NULL
    spdf.active_floodplain_elbe_csa$Shape_Leng <- NULL
    
    if (!"SECTION" %in% names(spdf.active_floodplain_elbe_csa)) {
        spdf.tiles <- readOGR(dsn = "~/flut3_Elbe/data/shp",
                              layer = "tiles", verbose = FALSE,
                              p4s = "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")
        spdf.tiles <- spdf.tiles[,c("NAME")]
        names(spdf.tiles) <- "SECTION"
        sf.af <- st_as_sf(spdf.active_floodplain_elbe_csa)
        sf.t <- st_as_sf(spdf.tiles)
        sf.aft <- st_join(sf.af, sf.t)
        sf.aft <- sf.aft[!grepl(".", row.names(sf.aft), fixed = TRUE), ]
        spdf.active_floodplain_elbe_csa <- as(sf.aft, "Spatial")
        rm(spdf.tiles, sf.af, sf.t, sf.aft)
    }
    
    spdf.active_floodplain_elbe_csa$SECTION <- as.character(spdf.active_floodplain_elbe_csa$SECTION)
    proj4string(spdf.active_floodplain_elbe_csa) <- 
        CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")
    
    if (!"SECTION_DO" %in% names(spdf.active_floodplain_elbe_csa)) {
        
        spdf.active_floodplain_elbe_csa$SECTION_DO <- 
            rep(NA_character_, nrow(spdf.active_floodplain_elbe_csa))
        
        spdf.tiles <- readOGR(dsn = "~/flut3_Elbe/data/shp",
                              layer = "tiles", verbose = FALSE,
                              p4s = "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")
        spdf.tiles <- spdf.tiles[,c("NAME")]
        names(spdf.tiles) <- "SECTION"
        
        for (i in 1:(nrow(spdf.tiles) - 1)) {
            # in section
            section <- spdf.tiles$SECTION[i]
            id_sec <- row.names(spdf.active_floodplain_elbe_csa[
                which(spdf.active_floodplain_elbe_csa$SECTION == section), ])
            
            # in downstream tile
            section_do <- spdf.tiles$SECTION[i + 1]
            spdf.tile_sel <- spdf.tiles[i + 1,]
            id_sel <- row.names(spdf.active_floodplain_elbe_csa[spdf.tile_sel,])
            
            id <- id_sec[id_sec %in% id_sel]
            spdf.active_floodplain_elbe_csa[id, "SECTION_DO"] <- as.character(section_do)
        }
    }
    
    # reorder columns
    spdf.active_floodplain_elbe_csa <- spdf.active_floodplain_elbe_csa[, c(
        "STATION", "STATION_INT", "SECTION", "SECTION_DO", "WSP_AENDER",
        "WST_05MNQ","WST_MNQ", "WST_05MQ", "WST_a", "WST_075MQ", "WST_b",
        "WST_MQ", "WST_c", "WST_2MQ", "WST_3MQ", "WST_d", "WST_e", "WST_MHQ",
        "WST_HQ2", "WST_f", "WST_HQ5","WST_g", "WST_h", "WST_HQ10", "WST_HQ15",
        "WST_HQ20", "WST_HQ25", "WST_HQ50", "WST_HQ75", "WST_HQ100", "WST_i",
        "WST_HQ150", "WST_HQ200", "WST_HQ300", "WST_HQ500")]
    
    # export
    proj4string(spdf.active_floodplain_elbe_csa) <- 
        CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")
    usethis::use_data(spdf.active_floodplain_elbe_csa,
                      overwrite = TRUE, compress = "bzip2")
    system("mv data/spdf.active_floodplain_elbe_csa.rda data-raw/")
    system("cp data-raw/spdf.active_floodplain_elbe_csa.rda ~/.hydflood/")
    
    # clean up
    rm(spdf.active_floodplain_elbe_csa)
    
} else {
    write("data-raw/spdf.active_floodplain_elbe_csa.rda exists already", 
          stderr())
}

