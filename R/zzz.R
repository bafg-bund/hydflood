
.onLoad <- function(libname, pkgname) {
    # load package data
    utils::data("spdf.active_floodplain_elbe", "spdf.active_floodplain_rhein", 
                package = pkgname, envir = parent.env(environment()))
    
}


.onUnload  <- function(libpath) {
    for (a_dataset in c("spdf.active_floodplain_elbe", 
                        "spdf.active_floodplain_rhein")){
        if (exists(a_dataset, envir = globalenv())){
            rm(list = a_dataset, envir = globalenv())
        }
    }
}


if(getRversion() >= "2.15.1"){
    utils::globalVariables(c("spdf.active_floodplain_elbe_csa", 
                             "spdf.active_floodplain_rhein_csa"))
}
