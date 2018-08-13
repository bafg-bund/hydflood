require("rgdal")
require("sp")

# store spdf.active_floodplain_rhein_csa as external dataset
if (!(file.exists("data/spdf.active_floodplain_rhein_csa.rda"))) {
    
    # import
    spdf.active_floodplain_rhein_csa <- readOGR(
        dsn = "data-raw", 
        layer = "active_floodplain_rhein_csa", 
        verbose = FALSE)
    
    # export
    devtools::use_data(spdf.active_floodplain_rhein_csa, 
                       pkg = ".", overwrite = TRUE, compress = "bzip2")
    
    # clean up
    rm(spdf.active_floodplain_rhein_csa)
    
} else {
    write("data/spdf.active_floodplain_rhein_csa.rda exists already", stderr())
}

