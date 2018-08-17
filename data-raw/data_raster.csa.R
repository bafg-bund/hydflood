
# store raster.csa as external dataset
if (!(file.exists("data/raster.csa.rda"))){
    
    if (!exists("lib")){
        v <- R.Version()
        lib <- paste0("~/R/", paste(sep = ".", v$major, v$minor))
    }
    require("rgdal", lib.loc = lib)
    require("sp", lib.loc = lib)
    require("raster", lib.loc = lib)
    
    # import
    raster.csa <- readAll(raster("data-raw/raster.csa.tif"))
    
    # export
    devtools::use_data(raster.csa, pkg = ".", overwrite = TRUE, 
                       compress = "bzip2")
    
    # clean up
    rm(raster.csa)
    
} else {
    write("data/raster.csa.rda exists already", stderr())
}

