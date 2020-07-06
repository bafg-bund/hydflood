#' @name hydflood_cache
#' @rdname hydflood_cache
#' @title Caching
#'
#' @description Manage cached `hydflood` files with \pkg{hoardr}
#'
#' @details The dafault cache directory is `~/.hydflood`, but you can set
#' your own path using `cache_path_set()`
#'
#' `cache_delete` only accepts 1 file name, while
#' `cache_delete_all` doesn't accept any names, but deletes all files.
#' For deleting many specific files, use `cache_delete` in a [lapply()]
#' type call
#'
#' @section Useful user functions:
#'
#' - `hydflood_cache$cache_path_get()` get cache path
#' - `hydflood_cache$cache_path_set()` set cache path
#' - `hydflood_cache$list()` returns a character vector of full path file names
#' - `hydflood_cache$files()` returns file objects with metadata
#' - `hydflood_cache$details()` returns files with details
#' - `hydflood_cache$delete()` delete specific files
#' - `hydflood_cache$delete_all()` delete all files, returns nothing
#'
#' @examples \dontrun{
#' hydflood_cache
#'
#' # list files in cache
#' hydflood_cache$list()
#'
#' # delete certain database files
#' # hydflood_cache$delete("file path")
#' # hydflood_cache$list()
#'
#' # delete all files in cache
#' # hydflood_cache$delete_all()
#' # hydflood_cache$list()
#'
#' # set a different cache path from the default
#' # hydflood_cache$cache_path_set(full_path = "/Foo/Bar")
#' }
NULL
