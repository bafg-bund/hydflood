require("rgdal")
require("sp")

# store spdf.active_floodplain_rhein as external dataset
if (!(file.exists("data/spdf.active_floodplain_rhein.rda"))) {
    
    # import
    spdf.active_floodplain_rhein <- readOGR(dsn = "data-raw", 
                                            layer = "active_floodplain_rhein", 
                                            verbose = FALSE)
    
    # export
    devtools::use_data(spdf.active_floodplain_rhein, pkg = ".", overwrite = TRUE, 
                       compress = "bzip2")
    
    # clean up
    rm(spdf.active_floodplain_rhein)
    
} else {
    write("data/spdf.active_floodplain_rhein.rda exists already", stderr())
}

