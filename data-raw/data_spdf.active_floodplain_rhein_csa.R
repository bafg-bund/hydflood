
# store spdf.active_floodplain_rhein_csa as external dataset
if (!(file.exists("data-raw/spdf.active_floodplain_rhein_csa.rda"))) {
    
    library("rgdal")
    library("sp")
    
    # import
    spdf.active_floodplain_rhein_csa <- readOGR(
        dsn = "data-raw", 
        layer = "active_floodplain_rhein_csa",
        verbose = FALSE)
    
    # export
    usethis::use_data(spdf.active_floodplain_rhein_csa,
                      overwrite = TRUE, compress = "bzip2")
    system("mv data/spdf.active_floodplain_rhein_csa.rda data-raw/")
    
    # clean up
    rm(spdf.active_floodplain_rhein_csa)
    
} else {
    write("data-raw/spdf.active_floodplain_rhein_csa.rda exists already",
          stderr())
}

