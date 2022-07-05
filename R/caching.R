#' @name hydflood_cache
#' @rdname hydflood_cache
#' @title Caching
#'
#' @description Manage cached \code{hydflood} files with \pkg{hoardr}
#'
#' @details The default cache directory is \code{~/.hydflood}, but you can set
#' your own path using \code{cache_path_set()}
#'
#' \code{cache_delete} only accepts 1 file name, while
#' \code{cache_delete_all} does not accept any names, but deletes all files.
#' For deleting many specific files, use \code{cache_delete} in a
#' \code{[lapply()]} type call.
#'
#' @section Useful user functions:
#'
#' - \code{hydflood_cache$cache_path_get()} get cache path
#' 
#' - \code{hydflood_cache$cache_path_set()} set cache path
#' 
#' - \code{hydflood_cache$list()} return a character vector of full path file names
#' 
#' - \code{hydflood_cache$files()} return file objects with metadata
#' 
#' - \code{hydflood_cache$details()} return files with details
#' 
#' - \code{hydflood_cache$delete()} delete specific files
#' 
#' - \code{hydflood_cache$delete_all()} delete all files, returns nothing
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
