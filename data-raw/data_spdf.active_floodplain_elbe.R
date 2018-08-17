
# store spdf.active_floodplain_elbe as external dataset
if (!(file.exists("data/spdf.active_floodplain_elbe.rda"))) {
    
    if (!exists("lib")){
        v <- R.Version()
        lib <- paste0("~/R/", paste(sep = ".", v$major, v$minor))
    }
    require("rgdal", lib.loc = lib)
    require("sp", lib.loc = lib)
    
    # import
    spdf.active_floodplain_elbe <- readOGR(dsn = "data-raw", 
                                           layer = "active_floodplain_elbe", 
                                           verbose = FALSE)
    
    # export
    devtools::use_data(spdf.active_floodplain_elbe, pkg = ".", overwrite = TRUE, 
                       compress = "bzip2")
    
    # clean up
    rm(spdf.active_floodplain_elbe)
    
} else {
    write("data/spdf.active_floodplain_elbe.rda exists already", stderr())
}

