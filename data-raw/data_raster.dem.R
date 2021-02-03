
# store raster.dem as external dataset
if (!(file.exists("data/raster.dem.rda"))){
    
    library("rgdal")
    library("sp")
    library("raster")
    
    # import
    raster.dem <- raster("data-raw/raster.dem.tif")
    
    # export
    usethis::use_data(raster.dem, overwrite = TRUE, compress = "bzip2")
    
    # clean up
    rm(raster.dem)
    
} else {
    write("data/raster.dem.rda exists already", stderr())
}

