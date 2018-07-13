
require("rgdal")
require("sp")
require("raster")

# store raster.dem as external dataset
if (!(file.exists("data/raster.dem.rda"))){
    
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

