#' @name hydRasterStack-class
#' @rdname hydRasterStack-class
#' @title S4 class containing a dem and a csa raster dataset as RasterStack
#' 
#' @description The S4 class \linkS4class{hydRasterStack} is inherited from the
#'   S4 class \code{RasterStack} and stores two \code{RasterLayer}'s: dem
#'   (digital elevation model) and csa (cross section areas.)
#' 
#' @details The S4 class \linkS4class{hydRasterStack} has been designed to
#'   structure and simplify processing of GIS-based flooding algorythms along
#'   German federal waterways Elbe and Rhine.
#'   
#'   A \linkS4class{hydRasterStack} is inherited from a \code{RasterStack} and
#'   stores exactly two \code{RasterLayer}'s: The first layer \code{dem} stores
#'   a **d**igital **e**levation **m**odel and the second layer \code{csa}
#'   stores a possible join field for 1D hydrologic information, the **c**ross
#'   **s**ection **a**reas. The coordinate reference system (crs) for the stack
#'   must be 'ETRS 1989 UTM 32N' for data along River Rhine or 'ETRS 1989 UTM
#'   33N' for data along the River Elbe. Both are officially recocknized
#'   standard projections of governmental GIS data in Germany and are commonly
#'   used by water and navigation authorities providing the data.
#'   
#'   The layer \code{dem} provides the local topography of riverine floodplains
#'   with dataType \code{FLT4S}. Official **dem-w**'s (**d**igital **e**levation
#'   **m**odell of a **w**aterway; **DGM-W**: **D**igitales
#'   **G**elände**m**odell des **W**asserlaufs) published by water and
#'   navigation authorities provide elevation data for terrestrial parts of
#'   floodplains from airborne laserscanning and data from hydrographic surveys
#'   for aquatic parts. These datasets are presently provided in the height
#'   reference system DHHN92 (Deutschen Haupthöhennetz 1992).
#'   
#'   The layer \code{csa} provides the join field for 1D hydrologic information
#'   with dataType \code{INT4S} and refers to the official waterway stationing
#'   along River Rhine and Elbe. Therefore values of \code{csa} are limited to a
#'   range between 336200 and 865700 for River Rhine (crs: 'ETRS 1989 UTM 32N' )
#'   and a range between 0 and 585700 for River Elbe (crs: 'ETRS 1989 UTM 33N'
#'   ). The cross section areas have been derived from cross section lines used
#'   as input data for 1D hydraulic model SOBEK forming the basis for the FLYS
#'   dataset \code{\link[hyd1d]{df.flys}}.
#' 
#' @references
#'   \insertRef{wsv_dgmw_2016}{hydflood3}
#'   
#'   \insertRef{busch_einheitliche_2009}{hydflood3}
#'   
#'   \insertRef{bundesanstalt_fur_gewasserkunde_flys_2016}{hydflood3}
#' 
#' @exportClass hydRasterStack
#' 
methods::setClass(
    Class     = "hydRasterStack",
    contains  = "RasterStack",
    prototype = raster::stack(),
    validity  = function(object) {
        
        ## vector and function to catch error messages
        errors <- character()
        l <- function(errors) {as.character(length(errors) + 1)}
        
        ## validity checks
        # n layers = 2
        if (raster::nlayers(object) == 2L) {
            errors <- c(errors, paste0("Error ", l(errors), ": The number of ",
                                       "RasterLayer's must be 2."))
        }
        
        # names of layers = c("dem", "csa")
        if (!all(names(object) == c("dem", "csa"))) {
            errors <- c(errors, paste0("Error ", l(errors), ": The names of Ra",
                                       "sterLayer's must be c('dem', 'csa')."))
        }
        
        # crs 
        # define standard projections
        etrs_1989_utm_32_string <- sp::CRS("+init=epsg:25832")
        etrs_1989_utm_33_string <- sp::CRS("+init=epsg:25833")
        
        obj_crs <- raster::crs(object)
        # check
        if ( !(raster::compareCRS(obj_crs, etrs_1989_utm_32_string)) & 
             !(raster::compareCRS(obj_crs, etrs_1989_utm_33_string))) {
            errors <- c(errors, paste0("Error ", l(errors), ": The project",
                                       "ion of a hydRasterStack must be",
                                       " either 'ETRS 1989 UTM 32N' or 'ET",
                                       "RS 1989 UTM 33N'."))
        }
        
        # # res
        # if (all(raster::res(object) != c(1, 1))) {
        #     errors <- c(errors, paste0("Error ", l(errors), ": The resolution ",
        #                                "of a hydRasterStack must be 1."))
        # }
        
        # dem dataType
        if ("dem" %in% names(object)) {
            if (raster::dataType(object$dem) != "FLT4S" &
                raster::dataType(object$dem) != "FLT8S") {
                errors <- c(errors, paste0("Error ", l(errors), ": The dataTyp",
                                           "e of the RasterLayer 'dem' must be",
                                           " 'FLT4S' or 'FLT8S'."))
            }
        }
        
        # csa dataType
        if ("csa" %in% names(object)) {
            if (raster::dataType(object$csa) != "INT4S") {
                errors <- c(errors, paste0("Error ", l(errors), ": The dataTyp",
                                           "e of the RasterLayer 'csa' must be",
                                           " 'INT4S'."))
            }
        }
        
        # csa range(station_int)
        if (raster::compareCRS(obj_crs, etrs_1989_utm_32_string)) {
            if (maxValue(object$csa) > 865700) {
                errors <- c(errors, paste0("Error ", l(errors), ": The maximum",
                                           " value of 'csa' must be 865700 or ",
                                           "below for River Rhine."))
            }
            if (minValue(object$csa) < 336200) {
                errors <- c(errors, paste0("Error ", l(errors), ": The minimum",
                                           " value of 'csa' must be 336200 or ",
                                           "above for River Rhine."))
            }
        }
        if (raster::compareCRS(obj_crs, etrs_1989_utm_33_string)) {
            if (maxValue(object$csa) > 585700) {
                errors <- c(errors, paste0("Error ", l(errors), ": The maximum",
                                           " value of 'csa' must be 585700 or ",
                                           "below for River Elbe."))
            }
            if (minValue(object$csa) < 0) {
                errors <- c(errors, paste0("Error ", l(errors), ": The minimum",
                                           " value of 'csa' must be 0 or ",
                                           "above for River Elbe."))
            }
        }
        
        ## return
        if (l(errors) != 1) {
            stop(paste0(errors, collapse = "\n  "))
        } else {
            TRUE
        }
    }
)


#' @name hydRasterStack
#' @rdname hydRasterStack
#' @title Initialize a hydRasterStack
#'
#' @description To initialize an object of class \code{hydRasterStack} this
#'   function should be used. It checks all the required input data, downloads 
#'   missing data automatically and validates the final object.
#'
#' @param filename_dem an optional argument of length 1 with type
#'   \code{"character"} specifying a filename of a **d**igital **e**levation
#'   **m**odel raster dataset.
#'   
#'   If the file exists it is imported via \code{\link[raster]{raster}} and used
#'   to build the \code{hydRasterStack}, potentially cropped by argument
#'   \code{ext}. If the dem file does not exist, data are downloaded
#'   automatically and exported using \code{\link[raster]{writeRaster}} and can
#'   be reused to accelerate later computations.
#'   
#'   An existing dataset must be either in the coordinate reference system (crs)
#'   'ETRS 1989 UTM 32N' for River Rhine or 'ETRS 1989 UTM 33N' for River Elbe.
#'   It must also overlap with the active floodplain
#'   (\code{\link{spdf.active_floodplain_rhein}} or
#'   \code{\link{spdf.active_floodplain_elbe}}) of the river selected through
#'   the crs.
#'   
#'   If argument \code{filename_csa} is specified and exists too, 
#'   coordinate reference system (\code{\link[raster]{crs}}), extent
#'   (\code{\link[raster]{extent}}) and resolution (\code{\link[raster]{res}})
#'   of both raster datasets must match.
#'   
#'   Supported file types are the ’native’ raster package format and 
#'   those that can be read via rgdal (see \code{\link[rgdal]{readGDAL}}).
#' 
#' @param filename_csa an optional argument of length 1 with type 
#'   \code{"character"} specifying a filename of a **c**ross **s**ection 
#'   **a**rea raster dataset. 
#'   
#'   If the file exists it is imported via \code{\link[raster]{raster}} and used
#'   to build the \code{hydRasterStack}, potentially cropped by argument
#'   \code{ext}. If the csa file does not exist, data are downloaded
#'   automatically and exported using \code{\link[raster]{writeRaster}} and can
#'   be reused to accelerate later computations.
#'   
#'   An existing dataset must be either in the coordinate reference system (crs)
#'   'ETRS 1989 UTM 32N' for River Rhine or 'ETRS 1989 UTM 33N' for River Elbe.
#'   It must also overlap with the active floodplain
#'   (\code{\link{spdf.active_floodplain_rhein}} or
#'   \code{\link{spdf.active_floodplain_elbe}}) of the river selected through
#'   the crs and be in the possible range of \code{station_int} values: Elbe (m
#'   0 - 585700), Rhine (m 336200 - 865700).
#'   
#'   If argument \code{filename_dem} is specified too, coordinate reference
#'   system (\code{\link[raster]{crs}}), extent (\code{\link[raster]{extent}})
#'   and resolution (\code{\link[raster]{res}}) of both raster datasets must
#'   match.
#'   
#'   Supported file types are the ’native’ raster package format and 
#'   those that can be read via rgdal (see \code{\link[rgdal]{readGDAL}}).
#' 
#' @param ext optional argument of type \code{\link[raster]{Extent}}. If neither 
#'   \code{filename_dem} nor \code{filename_csa} are specified, \code{extent} is 
#'   required to download the respective data and generate temporary dem and csa
#'   datasets. If either \code{filename_dem} or \code{filename_csa} or both are
#'   specified, \code{ext} must be within the extent of raster files. Then 
#'   it is used to \code{\link[raster]{crop}} the supplied data.
#' 
#' @param crs optional argument of type \code{CRS}. If neither 
#'   \code{filename_dem} nor \code{filename_csa} are specified, \code{crs} is 
#'   used to select the respective river (Elbe: 'ETRS 1989 UTM 33N'; Rhine: 
#'   'ETRS 1989 UTM 32N') and \code{\link[raster]{crop}} downloaded dem and csa
#'   by the given \code{ext}. If either \code{filename_dem} or 
#'   \code{filename_csa} or both are specified, \code{crs} must match their 
#'   coordinate reference systems otherwise an error is returned.
#' 
#' @param \dots additional parameters passed to 
#'   \code{\link[raster]{writeRaster}}.
#' 
#' @return The function produces an object of class
#'   \code{"hydRasterStack"} inherited from \code{RasterStack}
#'   contain digital elevation and cross section area raster data.
#'
#' @seealso \code{\link[raster]{stack}}, \code{\link[raster]{Raster-class}}, 
#'   \code{\link[raster]{raster}}, \code{\link[raster]{writeRaster}}, 
#'   \code{\link{flood3}}, \code{\link{spdf.active_floodplain_rhein}}, 
#'   \code{\link{spdf.active_floodplain_elbe}}
#' 
#' @references
#'   \insertRef{wsv_dgmw_2016}{hydflood3}
#'   
#'   \insertRef{brockmann_auswertung_2008}{hydflood3}
#'   
#'   \insertRef{brockmann_produktblatt_2012}{hydflood3}
#'   
#'   \insertRef{brockmann_digitales_2008}{hydflood3}
#'   
#'   \insertRef{smile_consult_gmbh_dgm-w_2011}{hydflood3}
#'   
#'   \insertRef{fugro-hgn_gmbh_aufbau_2011}{hydflood3}
#'   
#'   \insertRef{arge_vermessung_schmid_-_inphoris_aufbau_2012}{hydflood3}
#'   
#'   \insertRef{bundesanstalt_fur_gewasserkunde_flys_2016}{hydflood3}
#'   
#'   \insertRef{brunotte_flussauen_data_2009}{hydflood3}
#' 
#' @examples \dontrun{
#'   library(hydflood3)
#'   
#'   ext <- extent(436500, 438000, 5415000, 5416500)
#'   crs <- crs("+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")
#'   
#'   r <- hydRasterStack(ext = ext, crs = crs)
#'   r
#' }
#' 
#' @export
#' 
hydRasterStack <- function(filename_dem = '', filename_csa = '', ext, crs, ...) {
    
    #####
    # validate the input data
    ##
    # vector and function to catch error messages
    errors <- character()
    l <- function(errors) {as.character(length(errors) + 1)}
    
    # make parent environment accessible through the local environment
    e <- environment()
    p_env <- parent.env(e)
    
    ##
    # filename_dem
    if (missing(filename_dem)){
        file_exists_dem <- FALSE
        file_create_dem <- FALSE
        ext_int_dem <- FALSE
        crs_int_dem <- FALSE
    } else {
        if (class(filename_dem) != "character") {
            errors <- c(errors, paste0("Error ", l(errors), ": 'filename_dem' ",
                                       "must be type 'character'."))
        }
        if (length(filename_dem) != 1) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'filename_dem' ",
                                       "must have length 1."))
        }
        # 1st time error messages
        if (l(errors) != "1") {stop(paste0(errors, collapse="\n  "))}
        
        if (filename_dem == '') {
            file_exists_dem <- FALSE
            file_create_dem <- FALSE
            ext_int_dem <- FALSE
            crs_int_dem <- FALSE
        } else {
            if (file.exists(filename_dem)) {
                raster.dem <- raster::raster(x = filename_dem)
                file_exists_dem <- TRUE
                file_create_dem <- FALSE
                ext_int_dem <- raster::extent(raster.dem)
                crs_int_dem <- raster::crs(raster.dem)
                res_int_dem <- raster::res(raster.dem)
            } else {
                file_exists_dem <- FALSE
                file_create_dem <- TRUE
                ext_int_dem <- FALSE
                crs_int_dem <- FALSE
            }
        }
    }
    
    ## 
    # filename_csa
    if (missing(filename_csa)){
        file_exists_csa <- FALSE
        file_create_csa <- FALSE
        ext_int_csa <- FALSE
        crs_int_csa <- FALSE
    } else {
        if (class(filename_csa) != "character") {
            errors <- c(errors, paste0("Error ", l(errors), ": 'filename_csa' ",
                                       "must be type 'character'."))
        }
        if (length(filename_csa) != 1) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'filename_csa' ",
                                       "must have length 1."))
        }
        # 2nd time error messages
        if (l(errors) != "1") {stop(paste0(errors, collapse="\n  "))}
        
        if (filename_csa == '') {
            file_exists_csa <- FALSE
            file_create_csa <- FALSE
            ext_int_csa <- FALSE
            crs_int_csa <- FALSE
        } else {
            if (file.exists(filename_csa)) {
                raster.csa <- raster::raster(x = filename_csa)
                file_exists_csa <- TRUE
                file_create_csa <- FALSE
                ext_int_csa <- raster::extent(raster.csa)
                crs_int_csa <- raster::crs(raster.csa)
                res_int_csa <- raster::res(raster.csa)
            } else {
                file_exists_csa <- FALSE
                file_create_csa <- TRUE
                ext_int_csa <- FALSE
                crs_int_csa <- FALSE
            }
        }
    }
    
    ##
    # file extents (and resolution)
    if (!is.logical(ext_int_dem) & !is.logical(ext_int_csa)) {
        if (! ext_int_dem == ext_int_csa) {
            errors <- c(errors, paste0("Error ", l(errors), ": The extents of ",
                                       "'filename_dem' and 'filename_csa' have",
                                       " to match."))
        } else {
            ext_int_ras <- ext_int_dem
        }
        if (!all(res_int_dem == res_int_csa)) {
            errors <- c(errors, paste0("Error ", l(errors), ": The resolutions",
                                       " of 'filename_dem' and 'filename_csa' ",
                                       "have to match."))
        } else {
            res_int <- res_int_dem
        }
        rm(res_int_dem, res_int_csa)
    }
    if (!is.logical(ext_int_dem) & is.logical(ext_int_csa)) {
        ext_int_ras <- ext_int_dem
        res_int <- res_int_dem
        rm(res_int_dem)
    }
    if (is.logical(ext_int_dem) & !is.logical(ext_int_csa)) {
        ext_int_ras <- ext_int_csa
        res_int <- res_int_csa
        rm(res_int_csa)
    }
    if (is.logical(ext_int_dem) & is.logical(ext_int_csa)) {
        ext_int_ras <- FALSE
        res_int <- c(1, 1)
    }
    
    ##
    # file projections
    if (!is.logical(crs_int_dem) & !is.logical(crs_int_csa)) {
        if (!raster::compareCRS(crs_int_dem, crs_int_csa)) {
            errors <- c(errors, paste0("Error ", l(errors), ": The coordinate ",
                                       "reference systems of 'filename_dem' an",
                                       "d 'filename_csa' have to match."))
        } else {
            crs_int_ras <- crs_int_dem
        }
    }
    if (!is.logical(crs_int_dem) & is.logical(crs_int_csa)) {
        crs_int_ras <- crs_int_dem
    }
    if (is.logical(crs_int_dem) & !is.logical(crs_int_csa)) {
        crs_int_ras <- crs_int_csa
    }
    if (is.logical(crs_int_dem) & is.logical(crs_int_csa)) {
        crs_int_ras <- FALSE
    }
    rm(crs_int_csa, crs_int_dem)
    
    # 3rd time error messages
    if (l(errors) != "1") {stop(paste0(errors, collapse="\n  "))}
    
    ##
    # ext
    if (missing(ext) & is.logical(ext_int_ras)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The 'ext' ",
                                   "argument has to be supplied."))
    }
    if (missing(ext) & !is.logical(ext_int_ras)) {
        ext_int <- ext_int_ras
    }
    if (!missing(ext) & is.logical(ext_int_ras)) {
        if (class(ext) != "Extent") {
            errors <- c(errors, paste0("Error ", l(errors), ": 'ext' must ",
                                       "be type 'Extent'."))
        } else {
            ext_int <- ext
        }
    }
    if (!missing(ext) & !is.logical(ext_int_ras)) {
        if (ext == ext_int_ras) {
            ext_int <- ext
        } else if (ext <= ext_int_ras) {
            message("'ext' will be used to crop the supplied raster file(s).")
            ext_int <- ext
        } else {
            errors <- c(errors, paste0("Error ", l(errors), ": The supplied 'e",
                                       "xt' must be totally within the supplie",
                                       "d raster(s)."))
        }
    }
    if (exists("ext")) {rm(ext)}
    if (exists("ext_int_ras")) {rm(ext_int_ras)}
    
    # 4th time error messages
    if (l(errors) != "1") {stop(paste0(errors, collapse="\n  "))}
    
    ## 
    # crs
    # check existence
    if (missing(crs) & is.logical(crs_int_ras)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The 'crs' ",
                                   "argument has to be supplied."))
    }
    if (missing(crs) & !is.logical(crs_int_ras)) {
        crs_int <- crs_int_ras
    }
    if (!missing(crs) & is.logical(crs_int_ras)) {
        if (class(crs) != "CRS") {
            errors <- c(errors, paste0("Error ", l(errors), ": 'crs' must ",
                                       "be type 'CRS'."))
        } else {
            crs_int <- crs
        }
    }
    if (!missing(crs) & !is.logical(crs_int_ras)) {
        if (!raster::compareCRS(crs, crs_int_ras)) {
            errors <- c(errors, paste0("Error ", l(errors), ": The supplied 'c",
                                       "rs' does not agree with the crs suppli",
                                       "ed through the raster(s)."))
        } else {
            crs_int <- crs
        }
    }
    if (exists("crs")) {rm(crs)}
    if (exists("crs_int_ras")) {rm(crs_int_ras)}
    
    # 5th time error messages
    if (l(errors) != "1") {stop(paste0(errors, collapse="\n  "))}
    
    # check standard projections
    etrs_1989_utm_32_string <- sp::CRS("+init=epsg:25832")
    etrs_1989_utm_33_string <- sp::CRS("+init=epsg:25833")
    if ( !(raster::compareCRS(crs_int, etrs_1989_utm_32_string)) & 
         !(raster::compareCRS(crs_int, etrs_1989_utm_33_string))) {
        errors <- c(errors, paste0("Error ", l(errors), ": The supplied crs mu",
                                   "st be either 'ETRS 1989 UTM 32N' or 'ETRS ",
                                   "1989 UTM 33N'."))
    } else {
        if (raster::compareCRS(crs_int, etrs_1989_utm_32_string)) {
            zone <- "32"
            river <- "Rhein"
        } else if (raster::compareCRS(crs_int, 
                                      etrs_1989_utm_33_string)) {
            zone <- "33"
            river <- "Elbe"
        } else {
            stop(errors)
        }
    }
    rm(etrs_1989_utm_32_string, etrs_1989_utm_33_string)
    
    ##
    # in area
    # check position
    sp.ext <- extent2polygon(ext_int, crs_int)
    
    if (exists("river")) {
        # access the spdf.active_floodplain_* data
        active_floodplain <- paste0("spdf.active_floodplain_", tolower(river))
        if (exists(active_floodplain, where = p_env)){
            get(active_floodplain, envir = p_env)
        } else {
            utils::data(active_floodplain)
        }
        rm(active_floodplain)
        if (river == "Elbe") {
            l.over <- sp::over(sp.ext, spdf.active_floodplain_elbe, 
                               returnList = TRUE)
            if (! (length(unlist(l.over)) > 0)) {
                errors <- c(errors, paste0("Error ", l(errors), ": The selecte",
                                           "d processing area does NOT overlap",
                                           " with the active floodplain of Riv",
                                           "er Elbe."))
            }
            rm(l.over)
        } else if (river == "Rhein") {
            l.over <- sp::over(sp.ext, spdf.active_floodplain_rhein, 
                               returnList = TRUE)
            if (! (length(unlist(l.over)) > 0)) {
                errors <- c(errors, paste0("Error ", l(errors), ": The selecte",
                                           "d processing area does NOT overlap",
                                           " with the active floodplain of Riv",
                                           "er Rhine."))
            }
            rm(l.over)
        }
    }
    
    # 6th time error messages
    if (l(errors) != "1") {
        stop(paste0(errors, collapse="\n  "))
    } else {
        rm(l, errors)
    }
    
    #####
    # additional args
    args <- list(...)
    
    #####
    # get missing input data
    nrows <- as.integer((ext_int@ymax - ext_int@ymin) / res_int[2])
    ncols <- as.integer((ext_int@xmax - ext_int@xmin) / res_int[1])
    in_memory <- raster::canProcessInMemory(raster::raster(ext_int, 
                                                           nrows = nrows, 
                                                           ncols = ncols, 
                                                           crs = crs_int), 
                                            n = 2)
    
    ##
    # csa
    if (file_exists_csa) {
        if (ext_int <= ext_int_csa) {
            if (in_memory) {
                raster.csa <- raster::crop(raster.csa, ext_int)
            } else {
                tmp_csa <- raster::rasterTmpFile(prefix = "r_tmp_csa_")
                raster.csa <- raster::crop(raster.csa, ext_int, 
                                           filename = tmp_csa)
            }
        }
    }
    # if (file_exists_csa & ext_int == ext_int_csa) {
    #     if (!in_memory) {
    #         tmp_csa <- raster::rasterTmpFile(prefix = "r_tmp_csa_")
    #         raster.csa <- raster::crop(raster.csa, ext_int, filename = tmp_csa)
    #     }
    # }
    
    if (!file_exists_csa) {
        
        # download the packages csa_file, if it has not yet been stored locally,
        # and load it
        csa_dir <- paste0(find.package("hydflood3"), "/data-raw")
        dir.create(csa_dir, showWarnings = FALSE, recursive = TRUE)
        csa_file <- paste0(csa_dir, "/spdf.active_floodplain_", tolower(river), 
                           "_csa.rda")
        if (!file.exists(csa_file)) {
            url <- paste0("https://www.aqualogy.de/wp-content/uploads/bfg/spdf", 
                          ".active_floodplain_", tolower(river), "_csa.rda")
            utils::download.file(url, csa_file, quiet = TRUE)
        }
        load(csa_file)
        
        if (river == "Elbe") {
            assign("spdf.active_floodplain_csa", 
                   spdf.active_floodplain_elbe_csa)
        } else {
            assign("spdf.active_floodplain_csa", 
                   spdf.active_floodplain_rhein_csa)
        }
        
        # subset it to sp.ext
        spdf.csa <- raster::intersect(spdf.active_floodplain_csa, sp.ext)
        
        # identify relevant sections
        sections_sel <- unique(spdf.csa@data$SECTION)
        for (a_section in sort(sections_sel, decreasing = TRUE)) {
            if (a_section %in% unique(spdf.csa@data$SECTION_DO)) {
                id <- which(spdf.csa@data$SECTION_DO == a_section)
                id_min <- min(id)
                id_max <- max(id)
                if (id_min == 1 | id_max == nrow(spdf.csa@data)) {
                    sections_sel <- unique(spdf.csa@data$SECTION[-id])
                }
            }
        }
        
        if (length(sections_sel) > 5) {
            stop(paste0("Error: The choosen 'ext' is very large and covers mor",
                        "e than 5 sections\n used for tiled computations. Plea",
                        "se reduce the size of you computation extent."))
        }
        if (length(sections_sel) > 3) {
            warning(paste0("Error: The choosen 'ext' is large and covers more ",
                           "than 3 sections\n used for tiled computations. Ple",
                           "ase reduce the size of your extent to\n avoid over",
                           "ly long computation times."))
        }
        
        # convert it to a raster
        raster.csa <- raster::raster(ext_int, ncols = ncols, nrows = nrows, 
                                     crs = crs_int)
        if (file_create_csa) {
            raster.csa <- raster::rasterize(spdf.csa, raster.csa, 
                                            field = "STATION_IN", update = TRUE,
                                            updateValue = 'all',
                                            filename = filename_csa, ...)
        } else {
            if (in_memory) {
                raster.csa <- raster::rasterize(spdf.csa, raster.csa, 
                                                field = "STATION_IN", 
                                                update = TRUE,
                                                updateValue = 'all')
            } else {
                tmp_csa <- raster::rasterTmpFile(prefix = "r_tmp_csa_")
                raster.csa <- raster::rasterize(spdf.csa, raster.csa, 
                                                field = "STATION_IN", 
                                                update = TRUE,
                                                updateValue = 'all',
                                                filename = tmp_csa)
            }
        }
    }
    raster::dataType(raster.csa) <- "INT4S"
    
    ##
    # dem
    if (file_exists_dem) {
        if (ext_int <= ext_int_dem) {
            if (in_memory) {
                raster.dem <- raster::crop(raster.dem, ext_int)
            } else {
                tmp_dem <- raster::rasterTmpFile(prefix = "r_tmp_dem_")
                raster.dem <- raster::crop(raster.dem, ext_int, 
                                           filename = tmp_dem)
            }
        }
    }
    if (!file_exists_dem) {
        if (!exists("sections_sel")) {
            # download the packages csa_file, if it has not yet been stored 
            # locally, and load it
            csa_dir <- paste0(find.package("hydflood3"), "/data-raw")
            dir.create(csa_dir, showWarnings = FALSE, recursive = TRUE)
            csa_file <- paste0(csa_dir, "/spdf.active_floodplain_", 
                               tolower(river), "_csa.rda")
            if(!file.exists(csa_file)) {
                url <- paste0("https://www.aqualogy.de/wp-content/uploads/bfg/", 
                              "spdf.active_floodplain_", tolower(river), "_csa",
                              ".rda")
                utils::download.file(url, csa_file, quiet = TRUE)
            }
            load(csa_file)
            
            if (river == "Elbe") {
                assign("spdf.active_floodplain_csa", 
                       spdf.active_floodplain_elbe_csa)
            } else {
                assign("spdf.active_floodplain_csa", 
                       spdf.active_floodplain_rhein_csa)
            }
            
            # subset it to sp.ext
            spdf.csa <- raster::intersect(spdf.active_floodplain_csa, sp.ext)
            
            # identify relevant sections
            sections_sel <- unique(spdf.csa@data$SECTION)
            for (a_section in sort(sections_sel, decreasing = TRUE)) {
                if (a_section %in% unique(spdf.csa@data$SECTION_DO)) {
                    id <- which(spdf.csa@data$SECTION_DO == a_section)
                    id_min <- min(id)
                    id_max <- max(id)
                    if (id_min == 1 | id_max == nrow(spdf.csa@data)) {
                        sections_sel <- unique(spdf.csa@data$SECTION[-id])
                    }
                }
            }
            
            if (length(sections_sel) > 5) {
                stop(paste0("Error: The choosen 'ext' is very large and covers",
                            " more than 5 sections\n used for tiled computatio",
                            "ns. Please reduce the size of you computation ext",
                            "ent."))
            }
            if (length(sections_sel) > 3) {
                warning(paste0("Error: The choosen 'ext' is large and covers m",
                               "ore than 3 sections\n used for tiled computati",
                               "ons. Please reduce the size of your extent to",
                               "\n avoid overly long computation times."))
            }
        }
        
        # download relevant DEM datasets
        merge_rasters <- list()
        for (a_section in sections_sel) {
            tmp_dem <- raster::rasterTmpFile(prefix = "r_tmp_dem_")
            url <- paste0("http://hpc-service.bafg.de/~WeberA/hydflood3/downlo",
                          "ads/", tolower(river), "/", a_section, "_DEM.asc")
            utils::download.file(url, tmp_dem, quiet = TRUE)
            
            x <- raster::raster(x = tmp_dem, crs = crs_int)
            merge_rasters <- append(merge_rasters, x)
        }
        
        # rename objects in merge_rasters
        dem_names <-c(letters[24:26], letters[1:23])
        names(merge_rasters) <- dem_names[1:length(merge_rasters)]
        
        if (length(merge_rasters) > 1){
            if (file_create_dem) {
                merge_rasters[["filename"]] <- filename_dem
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
            raster.dem <- do.call("raster::merge", merge_rasters)
        } else {
            if (file_create_dem) {
                raster.dem <- raster::crop(merge_rasters$x, y = ext_int, 
                                           filename = filename_dem, ...)
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
    }
    
    # extend raster.dem with NA's, if it smaller than ext
    if (extent(raster.dem) <= ext) {
        if (file_create_dem) {
            raster.dem <- raster::extend(raster.dem, y = ext, value = NA, 
                                         filename = filename_dem, ...)
        } else {
            if (!in_memory) {
                tmp_dem <- raster::rasterTmpFile(prefix = "r_tmp_dem_")
                raster.dem <- raster::extend(raster.dem, y = ext, value = NA, 
                                             filename = tmp_dem)
            } else {
                raster.dem <- raster::extend(raster.dem, y = ext, value = NA)
            }
        }
    }
    
    #####
    # assemble and return the product
    x <- methods::new("hydRasterStack")
    x@layers <- list(raster.dem, raster.csa)
    x@layers[[1]]@data@names <- "dem"
    x@layers[[2]]@data@names <- "csa"
    x@nrows <- nrows
    x@ncols <- ncols
    x@extent <- ext_int
    x@crs <- crs_int
    return(x)
    
}

