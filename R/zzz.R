#hydflood_cache <- NULL

.onLoad <- function(libname, pkgname) {
    # load package data
    utils::data("spdf.active_floodplain_elbe", "spdf.active_floodplain_rhein", 
                "spdf.tiles_elbe", "spdf.tiles_rhein",
                package = pkgname, envir = parent.env(environment()))
    
    # data cache
    x <- hoardr::hoard()
    x$cache_path_set(full_path = "~/.hydflood")
    hydflood_cache <<- x
}


.onUnload  <- function(libpath) {
    for (a_dataset in c("spdf.active_floodplain_elbe", "spdf.tiles_elbe",
                        "spdf.active_floodplain_rhein", "spdf.tiles_rhein",
                        "hydflood_cache")){
        if (exists(a_dataset, envir = globalenv())){
            rm(list = a_dataset, envir = globalenv())
        }
    }
}

if(getRversion() >= "2.15.1"){
    utils::globalVariables(c("spdf.active_floodplain_elbe_csa",
                             "spdf.active_floodplain_rhein_csa",
                             "df.gauging_station_data"))
}
