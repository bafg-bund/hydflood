
# store spdf.active_floodplain_elbe as external dataset
if (!(file.exists("data/spdf.active_floodplain_elbe.rda"))) {
    
    require("rgdal")
    require("sp")
    
    # import
    spdf.active_floodplain_elbe <- readOGR(dsn = "data-raw", 
                                           layer = "active_floodplain_elbe", 
                                           verbose = FALSE)
    
    # export
    usethis::use_data(spdf.active_floodplain_elbe, pkg = ".", overwrite = TRUE, 
                      compress = "bzip2")
    
    # clean up
    rm(spdf.active_floodplain_elbe)
    
} else {
    write("data/spdf.active_floodplain_elbe.rda exists already", stderr())
}

