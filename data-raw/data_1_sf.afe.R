
# store sf.afe as external dataset
if (!(file.exists("data/sf.afe.rda"))) {
    
    # import
    sf.afe <- st_read("data-raw/active_floodplain_elbe.shp")
    
    # reset crs to overcome R CHECK note of non-ascii characters
    st_crs(sf.afe) <- ""
    
    # export
    usethis::use_data(sf.afe, overwrite = TRUE, compress = "bzip2")
    
    # clean up
    rm(sf.afe)
    
} else {
    write("data/sf.afe.rda exists already", stderr())
}

