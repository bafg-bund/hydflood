
# store spdf.active_floodplain_elbe as external dataset
if (!(file.exists("data/spdf.active_floodplain_elbe.rda"))) {
    
    library("rgdal")
    library("sp")
    
    # import
    spdf.active_floodplain_elbe <- readOGR(dsn = "data-raw",
                                           layer = "active_floodplain_elbe",
                                           verbose = FALSE)
    crs(spdf.active_floodplain_elbe) <- UTM33N
    
    # export
    usethis::use_data(spdf.active_floodplain_elbe, overwrite = TRUE,
                      compress = "bzip2")
    
    # clean up
    rm(spdf.active_floodplain_elbe)
    
} else {
    write("data/spdf.active_floodplain_elbe.rda exists already", stderr())
}

