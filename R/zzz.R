#hydflood_cache <- NULL

.onLoad <- function(libname, pkgname) {
    # load package data
    utils::data("df.pnv", "sf.afe", "sf.afr", "sf.tiles_elbe", "sf.tiles_rhine",
                package = pkgname, envir = parent.env(environment()))
    
    # data cache
    x <- hoardr::hoard()
    x$cache_path_set(full_path = "~/.hydflood")
    hydflood_cache <<- x
}

.onUnload  <- function(libpath) {
    for (a_dataset in c("df.pnv", "sf.afe", "sf.afr", "sf.tiles_elbe",
                        "sf.tiles_rhine", "hydflood_cache")){
        if (exists(a_dataset, envir = globalenv())){
            rm(list = a_dataset, envir = globalenv())
        }
    }
}

if(getRversion() >= "2.15.1") {
    utils::globalVariables(c("df.pnv", "sf.afe", "sf.afr", "sf.tiles_elbe",
                             "sf.tiles_rhine", "sf.afe_csa", "sf.afr_csa"))
}
