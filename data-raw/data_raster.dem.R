
# store raster.dem as external dataset
if (!(file.exists("data/raster.dem.rda"))){
    
    if (!exists("lib")){
        v <- R.Version()
        lib <- paste0("~/R/", paste(sep = ".", v$major, v$minor))
    }
    require("rgdal", lib.loc = lib)
    require("sp", lib.loc = lib)
    require("raster", lib.loc = lib)
    
    # import
    raster.dem <- readAll(raster("data-raw/raster.dem.tif"))
    
    # export
    devtools::use_data(raster.dem, pkg = ".", overwrite = TRUE, 
                       compress = "bzip2")
    
    # clean up
    rm(raster.dem)
    
} else {
    write("data/raster.dem.rda exists already", stderr())
}

