
# store spdf.active_floodplain_rhein_csa as external dataset
if (!(file.exists("data-raw/spdf.active_floodplain_rhein_csa.rda"))) {
    
    if (!exists("lib")){
        v <- R.Version()
        lib <- paste0("~/R/", paste(sep = ".", v$major, v$minor))
    }
    require("rgdal", lib.loc = lib)
    require("sp", lib.loc = lib)
    
    # import
    spdf.active_floodplain_rhein_csa <- readOGR(
        dsn = "data-raw", 
        layer = "active_floodplain_rhein_csa", 
        verbose = FALSE)
    
    # export
    devtools::use_data(spdf.active_floodplain_rhein_csa, pkg = ".", 
                       overwrite = TRUE, compress = "bzip2")
    system("mv data/spdf.active_floodplain_rhein_csa.rda data-raw/")
    
    # clean up
    rm(spdf.active_floodplain_rhein_csa)
    
} else {
    write("data-raw/spdf.active_floodplain_rhein_csa.rda exists already", 
          stderr())
}

