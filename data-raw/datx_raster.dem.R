
# store raster.dem as external dataset
if (!(file.exists("data/raster.dem.rda"))){
    
    # import
    raster.dem <- rast("data-raw/raster.dem.tif")
    
    # export
    usethis::use_data(raster.dem, overwrite = TRUE, compress = "bzip2")
    
    # clean up
    rm(raster.dem)
    
} else {
    write("data/raster.dem.rda exists already", stderr())
}

