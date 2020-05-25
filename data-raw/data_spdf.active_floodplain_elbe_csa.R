
# store spdf.active_floodplain_elbe_csa as external dataset
if (!(file.exists("data-raw/spdf.active_floodplain_elbe_csa.rda"))) {
    
    library("rgdal")
    library("sp")
    library("sf")
    
    # import
    spdf.active_floodplain_elbe_csa <- readOGR(dsn = "data-raw",
        layer = "active_floodplain_elbe_csa", verbose = FALSE)
    if (!"SECTION" %in% names(spdf.active_floodplain_elbe_csa)) {
        spdf.tiles <- readOGR(dsn = "~/flut3_Elbe/data/shp",
                              layer = "tiles", verbose = FALSE)
        spdf.tiles <- spdf.tiles[,c("NAME")]
        names(spdf.tiles) <- "SECTION"
        sf.af <- st_as_sf(spdf.active_floodplain_elbe_csa)
        sf.t <- st_as_sf(spdf.tiles)
        sf.aft <- st_join(sf.af, sf.t)
        sf.aft <- sf.aft[!grepl(".", row.names(sf.aft), fixed = TRUE), ]
        spdf.active_floodplain_elbe_csa <- as(sf.aft, "Spatial")
        rm(spdf.tiles, sf.af, sf.t, sf.aft)
    }
    
    # export
    usethis::use_data(spdf.active_floodplain_elbe_csa,
                      overwrite = TRUE, compress = "bzip2")
    system("mv data/spdf.active_floodplain_elbe_csa.rda data-raw/")
    
    # clean up
    rm(spdf.active_floodplain_elbe_csa)
    
} else {
    write("data-raw/spdf.active_floodplain_elbe_csa.rda exists already", 
          stderr())
}

