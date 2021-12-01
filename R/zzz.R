#hydflood_cache <- NULL

.onLoad <- function(libname, pkgname) {
    # load package data
    utils::data("sf.afe", "sf.afr", "sf.tiles_elbe", "sf.tiles_rhein",
                package = pkgname, envir = parent.env(environment()))
    
    # data cache
    x <- hoardr::hoard()
    x$cache_path_set(full_path = "~/.hydflood")
    hydflood_cache <<- x
}

.onUnload  <- function(libpath) {
    for (a_dataset in c("sf.afe", "sf.afr", "sf.tiles_elbe", "sf.tiles_rhein",
                        "hydflood_cache")){
        if (exists(a_dataset, envir = globalenv())){
            rm(list = a_dataset, envir = globalenv())
        }
    }
}

if(getRversion() >= "2.15.1"){
    utils::globalVariables(c("sf.afe", "sf.afr", "sf.tiles_elbe",
                             "sf.tiles_rhein", "sf.afe_csa", "sf.afr_csa"))
}
