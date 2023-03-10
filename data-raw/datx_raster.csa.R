
# store raster.csa as external dataset
if (!(file.exists("data/raster.csa.rda"))){
    
    # import
    raster.csa <- rast("data-raw/raster.csa.tif")
    
    # export
    usethis::use_data(raster.csa, overwrite = TRUE, compress = "bzip2")
    
    # clean up
    rm(raster.csa)
    
} else {
    write("data/raster.csa.rda exists already", stderr())
}

