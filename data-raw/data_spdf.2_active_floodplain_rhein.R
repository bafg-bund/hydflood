
# store spdf.active_floodplain_rhein as external dataset
if (!(file.exists("data/spdf.active_floodplain_rhein.rda"))) {
    
    library("rgdal")
    library("sp")
    
    # import
    spdf.active_floodplain_rhein <- readOGR(dsn = "data-raw",
                                            layer = "active_floodplain_rhein",
                                            verbose = FALSE)
    crs(spdf.active_floodplain_rhein) <- UTM32N
    
    # export
    usethis::use_data(spdf.active_floodplain_rhein, overwrite = TRUE,
                      compress = "bzip2")
    
    # clean up
    rm(spdf.active_floodplain_rhein)
    
} else {
    write("data/spdf.active_floodplain_rhein.rda exists already", stderr())
}

