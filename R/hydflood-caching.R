#' cache path clear
#' @export
#' @rdname hydflood_cache_clear-defunct
#' @param ... ignored
hydflood_cache_clear <- function(...) {
    .Defunct(msg = paste0("defunct, see hydflood_cache$delete() and hydflood_c",
                          "ache$delete_all()"))
}

#' cache list 
#' @export
#' @rdname hydflood_cache_list-defunct
#' @param ... ignored
hydflood_cache_list <- function(...) {
    .Defunct(msg = "defunct, see hydflood_cache$list()")
}
