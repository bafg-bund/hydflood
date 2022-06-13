
# store sf.afr as external dataset
if (!(file.exists("data/sf.afr.rda"))) {
    
    # import
    sf.afr <- st_read("data-raw/active_floodplain_rhein.shp")
    
    # reset crs to overcome R CHECK note of non-ascii characters
    st_crs(sf.afr) <- ""
    
    # export
    usethis::use_data(sf.afr, overwrite = TRUE, compress = "bzip2")
    
    # clean up
    rm(sf.afr)
    
} else {
    write("data/sf.afr.rda exists already", stderr())
}

