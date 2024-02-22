#' @name floodCharacteristicWaterlevel
#' @rdname floodCharacteristicWaterlevel
#' 
#' @title Function to compute flood extents \code{SpatRaster} for characteristic
#'   water levels
#' 
#' @description Computes flood extents of characteristic water levels for the
#'   active floodplains along the German federal waterways Elbe, Rhine and the
#'   North Sea estuaries based on 1d water levels computed by
#'   \code{\link[hyd1d]{waterLevelFlood2}} or 
#'   \code{\link[hyd1d]{waterLevelFlys3}} provided by package \pkg{hyd1d}.
#' 
#' @param x has to by type \code{SpatRaster} and has to include both input 
#'   raster layers \code{csa} (cross section areas) and \code{dem} (digital 
#'   elevation model). To compute water levels along the River Elbe \code{x} 
#'   has to be in the coordinate reference system 
#'   \href{https://spatialreference.org/ref/epsg/etrs89-utm-zone-33n/}{ETRS 1989 UTM 33N},
#'   for River Rhine and the estuaries in 
#'   \href{https://spatialreference.org/ref/epsg/etrs89-utm-zone-32n/}{ETRS 1989 UTM 32N}.
#'   Other coordinate reference systems are not permitted.
#' @param value an optional value of type \code{character}. Commonly available
#'   values are \code{c("MThw", "MTnw", "HThw", "NTnw", "HHW", "NNW", "MNW",
#'   "MW", "MHW")} or a column supplied in \code{df}.
#' @param df an optional object of type \code{data.frame}, which must contain
#'   the columns \code{gauging_station}, \code{river}, \code{longitude},
#'   \code{latitude}, \code{km_csa}, \code{pnp} and finally a water level column
#'   named in \code{value}.
#' @param shift an optional \code{numeric} constant to shift the water level
#'   value.
#' @param filename supplies an optional output filename and has to be type 
#'   \code{character}.
#' @param \dots additional arguments as for \code{\link[terra]{writeRaster}}.
#' 
#' @return \code{SpatRaster} object with a \code{numeric} flood extent in the
#'   range of \code{[0, 1]}.
#' 
#' @details For the characteristic water level provided in \code{value} (and 
#'   \code{df}) \code{floodCharacteristicWaterLevel()} computes a 1d water
#'   level using \code{\link[hyd1d]{waterLevelFlood2}} along the requested river
#'   section. This 1d water level is transfered to a \code{wl} (water level)
#'   raster layer, which is in fact a copy of the \code{csa} (cross section
#'   areas) layer, and then compared to the \code{dem} (digital elevation model)
#'   layer. Where the \code{wl} layer is higher than the \code{dem}, the
#'   resulting flood extent layer is set to 1.
#' 
#' @seealso \code{\link[hyd1d]{waterLevelFlood2}},
#'   \code{\link[terra]{writeRaster}},
#'   \code{\link[terra]{terraOptions}}
#' 
#' @references 
#'   \insertRef{weber_flood_estuary_2024}{hydflood}
#' 
#' @examples \donttest{
#'   options("hydflood.datadir" = tempdir())
#'   library(hydflood)
#'   
#'   # import the raster data and create a raster stack
#'   c <- st_crs("EPSG:25833")
#'   e <- ext(309000, 310000, 5749000, 5750000)
#'   x <- hydSpatRaster(ext = e, crs = c)
#'   
#'   # compute a flood extent
#'   fe <- floodCharacteristicWaterlevel(x = x, value = "MQ")
#'   
#'   # plot the product
#'   plot(fe)
#' }
#' 
#' @export
#' 
floodCharacteristicWaterlevel <- function(x, value = NULL, df = NULL,
                                          shift = NULL, filename = '', ...) {
    
    #####
    # check requirements
    ##
    # vector and function to catch error messages
    errors <- character()
    l <- function(errors) {as.character(length(errors) + 1)}
    
    ## x
    if (missing(x)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The 'x' ",
                                   "argument has to be supplied."))
    } else {
        # class
        if (!inherits(x, "SpatRaster")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'x' must be ",
                                       "type 'SpatRaster'."))
        }
        
        if (!all(c("dem", "csa") %in% names(x))) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'names(x)' must",
                                       " be 'dem' and 'csa'."))
        }
        
    }
    
    if (l(errors) != "1") {
        stop(paste0(errors, collapse="\n  "))
    }
    
    # crs
    if (! isUTM32(x) & !isUTM33(x)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The projection",
                                   " of x must be either 'ETRS 1989 UTM 32",
                                   "N' or 'ETRS 1989 UTM 33N'."))
    } else {
        if (isUTM32(x)) {
            bb <- rasterextent2polygon(x)
            if (nrow(sf.af("Rhine")[bb,]) > 0) {
                river <- "Rhine"
                tidal <- FALSE
                single <- TRUE
            } else {
                if (nrow(sf.af("estuaries")[bb,]) == 1) {
                    sf.estuaries_x <- sf.af("estuaries")[bb, ]
                    river <- sf.estuaries_x$name[1]
                    single <- TRUE
                } else {
                    sf.estuaries_x <- sf.af("estuaries")[bb,]
                    sf.estuaries_x <- suppressWarnings(
                        sf::st_intersection(sf.estuaries_x, bb))
                    sf.estuaries_x <- sf.estuaries_x[
                        order(sf::st_area(sf.estuaries_x), decreasing = TRUE), ]
                    river <- sf.estuaries_x$name[1]
                    single <- FALSE
                }
                tidal <- TRUE
            }
        } else if (isUTM33(x)) {
            river <- "Elbe"
            tidal <- FALSE
            single <- TRUE
        } else {
            stop(errors)
        }
    }
    
    if (l(errors) != "1") {
        stop(paste0(errors, collapse="\n  "))
    }
    
    ##
    # value
    if (is.null(df)) {
        if (is.null(value)) {
            stop("'value' must be supplied.")
        } else {
            if (!inherits(value, "character")) {
                stop("'value' must be type 'character'.")
            }
            if (length(value) != 1) {
                stop("The length of 'value' must 1.")
            }
            
            if (tidal) {
                charact_values <- c("MThw", "MTnw", "HThw", "NTnw", "HHW",
                                      "NNW", "MNW", "MW", "MHW")
            } else {
                charact_values <- unique(
                    hyd1d::df.flys$name[hyd1d::df.flys$river == river])
            }
            
            if (!value %in% charact_values) {
                stop(paste0("The supplied 'value' must be is among the followi",
                            "ng values:\n    '",
                            paste0(charact_values, collapse = "'\n    '"),
                            "'"))
            }
        }
    } else {
        if (!inherits(df, "data.frame")) {
            stop("'df' must be type 'data.frame'.")
        }
        if (!inherits(value, "character")) {
            stop("'value' must be type 'character'.")
        }
        if (length(value) != 1) {
            stop("The length of 'value' must 1.")
        }
        stopifnot("gauging_station" %in% names(df))
        stopifnot(inherits(df$gauging_station, "character"))
        stopifnot("river" %in% names(df))
        stopifnot(inherits(df$river, "character"))
        stopifnot(length(unique(df$river)) == 1)
        river_df <- unique(df$river)
        stopifnot(all(tolower(unique(df$river)) %in% 
                         unique(tolower(hyd1d::df.gauging_station_data$river))))
        stopifnot("longitude" %in% names(df))
        stopifnot(inherits(df$longitude, "numeric") |
                      inherits(df$longitude, "integer"))
        df$longitude <- as.numeric(df$longitude)
        stopifnot("latitude" %in% names(df))
        stopifnot(inherits(df$latitude, "numeric") |
                      inherits(df$latitude, "integer"))
        df$latitude <- as.numeric(df$latitude)
        stopifnot("km_csa" %in% names(df))
        stopifnot(inherits(df$km_csa, "numeric") |
                      inherits(df$km_csa, "integer"))
        df$km_csa <- as.numeric(df$km_csa)
        stopifnot("pnp" %in% names(df))
        stopifnot(inherits(df$pnp, "numeric") |
                      inherits(df$pnp, "integer"))
        df$pnp <- as.numeric(df$pnp)
        stopifnot(value %in% names(df))
        stopifnot(inherits(df[, value], "numeric") |
                      inherits(df[, value], "integer"))
        df[, value] <- as.numeric(df[, value])
        if (river_df != river) {
            message(paste0("The 'river' specified in 'df' overwrites the spati",
                           "ally selected."))
            river <- river_df
        }
    }
    
    ## shift
    if (! is.null(shift)) {
        stopifnot(inherits(shift, "numeric") | inherits(shift, "integer"))
        stopifnot(length(shift) == 1)
        shift <- as.numeric(shift)
    }
    
    ## filename
    if (! missing(filename)) {
        if (!inherits(filename, "character")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'filename' must",
                                       " be type 'character'."))
        }
        if (length(filename) != 1) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'filename' must",
                                       " have length 1."))
        }
    }
    
    #####
    # error messages
    if (l(errors) != "1") {
        stop(paste0(errors, collapse="\n  "))
    }
    
    #####
    # preprocessing
    #####
    # individual raster needed for the processing
    csa <- raster::raster(x$csa)
    dem <- raster::raster(x$dem)
    
    if (!single) {
        sf.estuaries_mask <- sf.estuaries_x[sf.estuaries_x$name == river, ]
        csa <- raster::raster(terra::mask(terra::rast(csa),
                                          terra::vect(sf.estuaries_mask)))
    }
    
    # out template
    out <- raster::raster(csa)
    
    # water level template
    waterlevel <- raster::raster(dem)
    
    ##
    # hyd1d-computation
    # initialize the WaterLevelDataFrame
    station_int <- stats::na.omit(as.integer(unlist(terra::unique(csa))))
    wldf_initial <- hyd1d::WaterLevelDataFrame(river = river,
                                               time = as.POSIXct(NA),
                                               station_int = station_int)
    
    # compute the 1d water level
    if (river %in% c("Rhine", "Elbe")) {
        wldf <- hyd1d::waterLevelFlys3(wldf_initial, name = value)
    } else {
        wldf <- hyd1d::waterLevelFlood2(wldf_initial, value = value, df = df)
    }
    
    # add the shift
    if (!is.null(shift)) {
        wldf$w <- wldf$w + shift
    }
    
    ##
    # raster processing
    # check memory requirements
    big <- !raster::canProcessInMemory(out, 4)
    filename <- raster::trim(filename)
    if (big & filename == '') {
        filename <- raster::rasterTmpFile()
    }
    if (filename != '') {
        out <- terra::writeStart(out, filename, ...)
        todisk <- TRUE
    } else {
        vv <- matrix(ncol = nrow(out), nrow = ncol(out))
        todisk <- FALSE
    }
    
    # initialize
    bs <- raster::blockSize(csa)
    pb <- raster::pbCreate(bs$n, ...)
    
    if (todisk) {
        for (i in 1:bs$n) {
            # vectorize cross section areas (integer)
            v_csa <- raster::getValues(csa, row = bs$row[i],
                                       nrows = bs$nrows[i])
            # get unique stations for the csa subset
            v_stations <- stats::na.omit(unique(v_csa))
            
            # copy v_csa to v_fe to create a template results vector with the 
            # same size and type
            v_fe <- rep(0, length(v_csa))
            
            # vectorize digital elevation model (numeric)
            v_dem <- raster::getValues(dem, row = bs$row[i], nrows = bs$nrows[i])
            
            # create an empty water level vector
            v_wl <- rep(-999, length(v_csa))
            
            # handle NA's
            id_na <- is.na(v_csa) | is.na(v_dem) 
            id_nona <- !id_na
            
            # transfer the water level info to v_wl
            for (a_station in v_stations) {
                v_wl[v_csa == a_station] <- 
                    wldf$w[wldf$station_int == a_station]
            }
            
            # compare the water level raster to the dem
            v_fe[id_nona][v_dem[id_nona] < v_wl[id_nona]] <- 
                v_fe[id_nona][v_dem[id_nona] < v_wl[id_nona]] + 1
            
            # transfer NA's
            v_fe[id_na] <- NA
            
            # write the resulting flood durations into out
            out <- terra::writeValues(out, v_fe, start = bs$row[i],
                                      nrows = bs$nrows[i])
            raster::pbStep(pb, i)
        }
        out <- terra::writeStop(out)
    } else {
        for (i in 1:bs$n) {
            # vectorize cross section areas (integer)
            v_csa <- raster::getValues(csa, row = bs$row[i],
                                       nrows = bs$nrows[i])
            # get unique stations for the csa subset
            v_stations <- stats::na.omit(unique(v_csa))
            # copy v_csa to v_fe to create a template results vector with the 
            # same size and type
            v_fe <- rep(0, length(v_csa))
            
            # vectorize digital elevation model (numeric)
            v_dem <- raster::getValues(dem, row = bs$row[i],
                                       nrows = bs$nrows[i])
            # copy v_dem to v_fwl to create a template vector with the same 
            # size and type
            v_wl <- rep(-999, length(v_csa))
            
            # handle NA's
            id_na <- is.na(v_csa) | is.na(v_dem) 
            id_nona <- !id_na
            
            # transfer the water level info to v_wl
            for (a_station in v_stations) {
                v_wl[v_csa == a_station] <- 
                    wldf$w[wldf$station_int == a_station]
            }
            
            # compare the water level raster to the dem
            v_fe[id_nona][v_dem[id_nona] < v_wl[id_nona]] <- 
                v_fe[id_nona][v_dem[id_nona] < v_wl[id_nona]] + 1
            
            # transfer NA's
            v_fe[id_na] <- NA
            
            cols <- bs$row[i]:(bs$row[i] + bs$nrows[i]-1)
            vv[,cols] <- matrix(v_fe, nrow = out@ncols)
            raster::pbStep(pb, i)
        }
        out <- raster::setValues(out, as.vector(vv))
    }
    raster::pbClose(pb)
    
    out <- terra::rast(out)
    
    # make the raster categorical
    cls <- data.frame(value = c(0, 1), flooding = c("dry", "wet"))
    terra::set.cats(out, value = cls[, c("value", "flooding")])
    names(out) <- "flood_extent"
    
    # append colortab
    terra::coltab(out) <- data.frame(value = c(0, 1), col = c("grey95", "blue"))
    
    return(out)
}

