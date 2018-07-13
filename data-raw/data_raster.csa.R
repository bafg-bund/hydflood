
require("rgdal")
require("sp")
require("raster")

# store raster.csa as external dataset
if (!(file.exists("data/raster.csa.rda"))){
    
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

