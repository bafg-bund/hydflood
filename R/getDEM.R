#' @name getDEM
#' @rdname getDEM
#' 
#' @title Function to obtain the digital elevation models for the active
#'   floodplains along German federal waterways Elbe and Rhine
#' 
#' @description This function downloads and patches the tiled digital elevation
#'   models (dem) along German federal waterways Elbe and Rhine that have been
#'   published on \href{https://www.pangaea.de}{pangaea.de}.
#' 
#' @param filename supplies an optional in- and output filename and has to be
#'   type \code{character}.
#' @param ext argument of type \code{\link[raster]{extent}}.
#' @param crs argument of type \code{\link[rgdal]{CRS}}. It is 
#'   used to select the respective river (Elbe: 'ETRS 1989 UTM 33N'; Rhine: 
#'   'ETRS 1989 UTM 32N')
#' @param \dots additional arguments as for \code{\link[raster]{writeRaster}}.
#' 
#' @return Raster* object containing elevation data for the selected floodplain
#'   region.
#' 
#' @references 
#'   \insertRef{weber_dgms_2020}{hydflood}
#'   
#'   \insertRef{weber_dgm_elbe_2020}{hydflood}
#'   
#'   \insertRef{weber_dgm_rhine_2020}{hydflood}
#' 
#' @examples \dontrun{
#' library(hydflood)
#' 
#' dem <- getDEM(ext = extent(c(309000, 310000, 5749000, 5750000)),
#'               crs = crs("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"))
#' }
#' 
#' @export
#' 
getDEM <- function(filename = '', ext, crs, ...) {
    
    #####
    # validate the input data
    ##
    # vector and function to catch error messages
    errors <- character()
    l <- function(errors) {as.character(length(errors) + 1)}
    
    ##
    # filename
    if (class(filename) != "character") {
        errors <- c(errors, paste0("Error ", l(errors), ": 'filename' ",
                                   "must be type 'character'."))
    }
    if (length(filename) != 1) {
        errors <- c(errors, paste0("Error ", l(errors), ": 'filename' ",
                                   "must have length 1."))
    }
    
    if (l(errors) != "1") {stop(paste0(errors, collapse="\n  "))}
    
    if (filename == '') {
        if (missing(ext) & missing(crs)) {
            stop(paste0("Error 1: If you don't provide an existing 'filename',",
                        " you have to specify 'ext' and 'crs'."))
        }
        file_exists_dem <- FALSE
        file_create_dem <- FALSE
        ext_int_dem <- FALSE
        crs_int_dem <- FALSE
    } else {
        if (file.exists(filename)) {
            file_exists_dem <- TRUE
            file_create_dem <- FALSE
            ext_int_dem <- TRUE
            crs_int_dem <- TRUE
            raster.dem <- raster::raster(x = filename)
            ext_dem <- raster::extent(raster.dem)
            crs_dem <- raster::crs(raster.dem)
            res_dem <- raster::res(raster.dem)
        } else {
            file_exists_dem <- FALSE
            file_create_dem <- TRUE
            ext_int_dem <- FALSE
            crs_int_dem <- FALSE
        }
    }
    
    ##
    # crs
    if (missing(crs)) {
        if (crs_int_dem) {
            crs_int <- crs_dem
        } else {
            errors <- c(errors, paste0("Error ", l(errors), ": If 'filename' d",
                                       "oes not provide a CRS, you must specif",
                                       "y 'crs'."))
            stop(paste0(errors, collapse="\n  "))
        }
    }
    if (!missing(crs)) {
        if (class(crs)[[1]] != "CRS") {
            errors <- c(errors, paste0("Error ", l(errors), ": 'crs' must be t",
                                       "ype 'CRS'."))
            stop(paste0(errors, collapse="\n  "))
        }
        if (crs_int_dem) {
            if (!raster::compareCRS(crs, crs_dem)) {
                errors <- c(errors, paste0("Error ", l(errors), ": The supplie",
                                           "d 'crs' does not agree with the cr",
                                           "s of the raster supplied through '",
                                           "filename'."))
                stop(paste0(errors, collapse="\n  "))
            }
            crs_int <- crs_dem
        } else {
            crs_int <- crs
        }
    }
    
    # check standard projections
    if (!isUTM32(crs_int) & !isUTM33(crs_int)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The supplied 'crs' ",
                                   "must be either 'ETRS 1989 UTM 32N' or 'ETR",
                                   "S 1989 UTM 33N'."))
        stop(paste0(errors, collapse="\n  "))
    } else {
        if (isUTM32(crs_int)) {
            zone <- "32"
            river <- "Rhein"
            crs_int <- sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")
        } else if (isUTM33(crs_int)) {
            zone <- "33"
            river <- "Elbe"
            crs_int <- sp::CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")
        } else {
            stop(errors)
        }
    }
    
    ##
    # ext
    crop <- FALSE
    if (missing(ext)) {
        if (ext_int_dem) {
            ext_int <- ext_dem
        } else {
            errors <- c(errors, paste0("Error ", l(errors), ": If 'filename' d",
                                       "oes not provide an extent, so you must",
                                       " specify the 'ext'."))
            stop(paste0(errors, collapse="\n  "))
        }
    }
    if (!missing(ext)) {
        if (class(ext) != "Extent") {
            errors <- c(errors, paste0("Error ", l(errors), ": 'ext' must ",
                                       "be type 'Extent'."))
            stop(paste0(errors, collapse="\n  "))
        }
        if (ext_int_dem) {
            if (ext == ext_dem) {
                ext_int <- ext
            } else if (ext <= ext_dem) {
                message("'ext' will be used to crop the supplied raster file.")
                ext_int <- ext
                crop <- TRUE
            } else {
                errors <- c(errors, paste0("Error ", l(errors), ": The supplie",
                                           "d 'ext' must be totally within the",
                                           " raster supplied through 'filename",
                                           "'."))
                stop(paste0(errors, collapse="\n  "))
            }
        } else {
            ext_int <- ext
        }
    }
    
    ##
    # in area
    # check position
    if (exists("river")) {
        # access the spdf.active_floodplain_* data
        active_floodplain <- paste0("spdf.active_floodplain_", tolower(river))
        if (exists(active_floodplain, where = parent.env(environment()))){
            get(active_floodplain, envir = parent.env(environment()))
        } else {
            utils::data(active_floodplain)
        }
        rm(active_floodplain)
        sp.ext <- extent2polygon(ext_int, crs_int)
        if (river == "Elbe") {
            raster::crs(spdf.active_floodplain_elbe) <- crs_int
            l.over <- sp::over(sp.ext, spdf.active_floodplain_elbe, 
                               returnList = TRUE)
            if (! (length(unlist(l.over)) > 0)) {
                errors <- c(errors, paste0("Error ", l(errors), ": The selecte",
                                           "d 'ext' does NOT overlap with the ",
                                           "active floodplain of River Elbe."))
            }
            rm(l.over)
        } else if (river == "Rhein") {
            raster::crs(spdf.active_floodplain_rhein) <- crs_int
            l.over <- sp::over(sp.ext, spdf.active_floodplain_rhein, 
                               returnList = TRUE)
            if (! (length(unlist(l.over)) > 0)) {
                errors <- c(errors, paste0("Error ", l(errors), ": The selecte",
                                           "d 'ext' does NOT overlap with the ",
                                           "active floodplain of River Rhine."))
            }
            rm(l.over)
        }
    }
    
    # error messages
    if (l(errors) != "1") {
        stop(paste0(errors, collapse="\n  "))
    } else {
        rm(l, errors)
    }
    
    #####
    # additional args
    args <- list(...)
    overwrite <- FALSE
    if ("overwrite" %in% names(args)) {
        overwrite <- args[["overwrite"]]
        if (overwrite) {
            file_create_dem <- TRUE
        }
    }
    
    #####
    # processing
    if (crop) {
        return(raster::crop(raster.dem, sp.ext))
    }
    if (file.exists(filename) & missing(ext) & missing(crs)) {
        return(raster.dem)
    }
    if (file.exists(filename)) {
        if (raster::extent(raster::raster(filename)) == ext_int) {
            return(raster.dem)
        }
    }
    
    nrows <- as.integer((ext_int@ymax - ext_int@ymin))
    ncols <- as.integer((ext_int@xmax - ext_int@xmin))
    in_memory <- raster::canProcessInMemory(raster::raster(ext_int,
                                                           nrows = nrows,
                                                           ncols = ncols,
                                                           crs = crs_int),
                                            n = 2)
    
    if (river == "Elbe") {
        within <- rgeos::gContains(spdf.tiles_elbe, sp.ext, byid = TRUE)[,1]
        if (any(within)) {
            spdf.tiles <- spdf.tiles_elbe[within,]
        } else {
            spdf.tiles <- spdf.tiles_elbe[sp.ext,]
        }
    } else {
        within <- rgeos::gContains(spdf.tiles_rhein, sp.ext, byid = TRUE)[,1]
        if (any(within)) {
            spdf.tiles <- spdf.tiles_rhein[within,]
        } else {
            spdf.tiles <- spdf.tiles_rhein[sp.ext,]
        }
    }
    
    if (length(spdf.tiles) > 5) {
        stop(paste0("Error: The choosen 'ext' is very large and covers more th",
                    "an 5 DEM tiles.\n   Please reduce the size of your extent",
                    "."))
    }
    if (length(spdf.tiles) > 3) {
        warning(paste0("Error: The choosen 'ext' is large and covers more than",
                       " 3 DEM tiles.\n   Please reduce the size of your exten",
                       "t to avoid overly long computation times."))
    }
    
    if (!dir.exists(hydflood_cache$cache_path_get())) {
        dir.create(hydflood_cache$cache_path_get(), FALSE, TRUE)
    }
    merge_rasters <- list()
    for (i in 1:length(spdf.tiles)) {
        file <- paste0(hydflood_cache$cache_path_get(), "/", spdf.tiles$name[i],
                       "_DEM.tif")
        if (!file.exists(file)) {
            utils::download.file(spdf.tiles$url[i], file, quiet = TRUE)
        }
        merge_rasters <- append(merge_rasters, raster::raster(x = file))
    }
    
    # rename objects in merge_rasters
    dem_names <-c(letters[24:26], letters[1:23])
    names(merge_rasters) <- dem_names[1:length(merge_rasters)]
    
    if (length(merge_rasters) > 1){
        if (file_create_dem) {
            merge_rasters[["filename"]] <- filename
            if (length(args) > 0){
                for (i in 1:length(args)){
                    merge_rasters[[names(args)[i]]] <- args[i]
                }
            }
        } else {
            if (!in_memory) {
                tmp_dem <- raster::rasterTmpFile(prefix = "r_tmp_dem_")
                merge_rasters[["filename"]] <- tmp_dem
            }
        }
        merge_rasters[["overlap"]] <- TRUE
        merge_rasters[["ext"]] <- ext_int
        if (overwrite) {
            merge_rasters[["overwrite"]] <- TRUE
        }
        raster.dem <- do.call("merge", merge_rasters)
    } else {
        if (file_create_dem) {
            raster.dem <- raster::crop(merge_rasters$x, y = ext_int)
            if (!file.exists(filename) | overwrite) {
                raster::writeRaster(raster.dem, filename = filename,
                                    ...)
            }
        } else {
            if (!in_memory) {
                tmp_dem <- raster::rasterTmpFile(prefix = "r_tmp_dem_")
                raster.dem <- raster::crop(merge_rasters$x, y = ext_int,
                                           filename = tmp_dem)
            } else {
                raster.dem <- raster::crop(merge_rasters$x, y = ext_int)
            }
        }
    }
    
    return(raster.dem)
    
}
