#' @name flood3Points
#' @rdname flood3Points
#' 
#' @title Function to compute flood duration for point coordinates along German
#'   federal waterways Elbe and Rhine using the 1D water level algorythms
#'   \code{hyd1d::waterLevel()} and \code{hyd1d::waterLevelPegelonline()}
#' 
#' @description Computes flood duration for points located in the active
#'   floodplains along German federal waterways Elbe  and Rhine based on 1D
#'   water levels computed by \code{\link[hyd1d]{waterLevel}} or
#'   \code{\link[hyd1d]{waterLevelPegelonline}} provided by package
#'   \href{https://cran.r-project.org/package=hyd1d}{hyd1d}.
#' 
#' @param x has to by type \code{SpatialPoints} or \code{SpatialPointsDataFrame}
#'   possibly including columns \code{csa} (cross section areas) and \code{dem}
#'   (digital elevation model). To compute water levels along the River Elbe
#'   \code{x} has to be in the coordinate reference system 
#'   \href{http://spatialreference.org/ref/epsg/etrs89-utm-zone-33n/}{ETRS 1989 UTM 33N},
#'   for River Rhine in 
#'   \href{http://spatialreference.org/ref/epsg/etrs89-utm-zone-32n/}{ETRS 1989 UTM 32N}.
#'   Other coordinate reference systems are not permitted.
#' @param seq has to be type \code{c("POSIXct", "POSIXt")} or \code{Date} and
#'   have length larger 0. If \code{seq} is type \code{c("POSIXct", "POSIXt")},
#'   values must be in the temporal range between 31 days ago (\code{Sys.time()
#'   - 2678400}) and now (\code{Sys.time()}). Then
#'   \code{\link[hyd1d]{waterLevelPegelonline}} is used internally for the water
#'   level computations. If \code{seq} is type \code{Date} values must be in the
#'   temporal range between 1990-01-01 and yesterday (\code{Sys.Date() - 1})
#' 
#' @return \code{SpatialPointsDataFrame} with flood duration stored in column
#'   `flood3` in the range of \code{[0, length(seq)]}, elevation stored in
#'   column \code{dem} and cross section areas stored in column \code{csa}.
#' 
#' @details For every time step provided in \code{seq} \code{flood3Points()}
#'   computes a 1D water level along the requested river section. This 1D water
#'   level is transfered to a temporary \code{wl} (water level) column and then
#'   compared to the \code{dem} (digital elevation model) column. Where the
#'   \code{wl} is higher than the \code{dem} flood duration \code{flood3} is
#'   increased by 1.
#' 
#' @seealso \code{\link[hyd1d]{waterLevel}},
#'   \code{\link[hyd1d]{waterLevelPegelonline}},
#'   \code{\link[sp]{SpatialPointsDataFrame-class}}
#' 
#' @examples \dontrun{
#' library(hydflood)
#' 
#' # create a random SpatialPoints object
#' c <- crs("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")
#' e <- extent(309000, 310000, 5749000, 5750000)
#' sp.area <- hydflood:::extent2polygon(e, c)
#' set.seed(123)
#' sp <- spsample(sp.area, n = 10, "random")
#' 
#' # create a temporal sequence
#' seq <- seq(as.Date("2016-12-01"), as.Date("2016-12-31"), by = "day")
#' 
#' # compute a flood duration
#' spdf <- flood3Points(x = sp, seq = seq)
#' }
#' 
#' @export
#' 
flood3Points <- function(x, seq) {
    
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
        if (! class(x)[1] %in% c("SpatialPoints" , "SpatialPointsDataFrame")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'x' must be eit",
                                       "her type 'SpatialPoints' or 'SpatialPo",
                                       "intsDataFrame'."))
        }
        
        # crs
        if ( !isUTM32(x@proj4string) & !isUTM33(x@proj4string)) {
            errors <- c(errors, paste0("Error ", l(errors), ": The projection",
                                       " of x must be either 'ETRS 1989 UTM 32",
                                       "N' or 'ETRS 1989 UTM 33N'."))
        } else {
            if (isUTM32(x@proj4string)) {
                zone <- "32"
                river <- "Rhein"
                raster::crs(x) <- utm32n
                # within <- rgeos::gContains(spdf.tiles_rhein, x, byid = TRUE)[,1]
                # if (any(within)) {
                #     spdf.tiles <- spdf.tiles_rhein[within,]
                # } else {
                #     spdf.tiles <- spdf.tiles_rhein[x,]
                # }
                spdf.tiles <- spdf.tiles_rhein[x,]
            } else if (isUTM33(x@proj4string)) {
                zone <- "33"
                river <- "Elbe"
                raster::crs(x) <- utm33n
                # within <- rgeos::gContains(x, spdf.tiles_elbe, byid = TRUE)[,1]
                # if (any(within)) {
                #     spdf.tiles <- spdf.tiles_elbe[within,]
                # } else {
                #     spdf.tiles <- spdf.tiles_elbe[x,]
                # }
                spdf.tiles <- spdf.tiles_elbe[x,]
            } else {
                stop(errors)
            }
            # rm(within)
        }
        
        # check position
        if (exists("river")) {
            # access the spdf.active_floodplain_* data
            active_floodplain <- paste0("spdf.active_floodplain_",
                                        tolower(river))
            get(active_floodplain, pos = -1)
            if (river == "Elbe") {
                l.over <- x[spdf.active_floodplain_elbe,]
                if (length(l.over) == 0) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'x' doe",
                                               "s NOT overlap with the active ",
                                               "floodplain of River Elbe."))
                } else if (length(l.over) < length(x)) {
                    message(paste0("'x' does not fully overlap with the active",
                                   " floodplain of River Elbe.\nFlood duration",
                                   "s are computed only for overlapping points",
                                   "."))
                }
            } else if (river == "Rhein") {
                l.over <- x[spdf.active_floodplain_rhein,]
                if (length(l.over) == 0) {
                    errors <- c(errors, paste0("Error ", l(errors), ": 'x' doe",
                                               "s NOT overlap with the active ",
                                               "floodplain of River Rhein."))
                } else if (length(l.over) < length(x)) {
                    message(paste0("'x' does not fully overlap with the active",
                                   " floodplain of River Rhine.\nFlood duratio",
                                   "ns are computed only for overlapping point",
                                   "s."))
                }
            }
            rm(l.over)
        }
    }
    
    ## seq
    if (missing(seq)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The 'seq' argument ",
                                   "has to be supplied."))
    } else {
        # class
        if (! (all(class(seq) == "Date")) &
            ! (all(class(seq) == c("POSIXct", "POSIXt")))) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'seq' must be e",
                                       "ither type 'Date' or c('POSIXct', 'POS",
                                       "IXt')."))
        }
        # length
        if (length(seq) < 1L) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'seq' must have",
                                       " length larger 0."))
        }
        # NA and possible range
        if (any(is.na(seq))){
            errors <- c(errors, paste0("Error ", l(errors), ": 'seq' or elemen",
                                       "ts of it must not be NA."))
        } else {
            time_min <- trunc(Sys.time() - as.difftime(31, units = "days"),
                              units = "days")
            if (all(class(seq) == c("POSIXct", "POSIXt"))) {
                if (any(seq < time_min)) {
                    errors <- c(errors, paste0("Error ", l(errors), ": Values ",
                                               "of 'seq' must be between ",
                                               format(time_min, "%Y-%m-%d"),
                                               " 00:00:00 and now, if type of ",
                                               "'seq' is c('POSIXct', 'POSIXt'",
                                               ")."))
                }
                type_date <- FALSE
            }
            if (all(class(seq) == "Date")) {
                if (any(seq < as.Date("1990-01-01")) |
                    any(seq > Sys.Date() - 1)) {
                    errors <- c(errors, paste0("Error ", l(errors), ": Values ",
                                               "of 'seq' must be between 1990-",
                                               "01-01 and yesterday."))
                }
                type_date <- TRUE
            }
            rm(time_min)
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
    # get columns 'dem' and 'csa'
    if (class(x)[1] == "SpatialPoints") {
        x <- sp::SpatialPointsDataFrame(sp::coordinates(x),
            data.frame(id_tmp = 1:length(x),
                       tile_name = rep(NA_character_, length(x)),
                       stringsAsFactors = FALSE),
            proj4string = x@proj4string)
    } else {
        if (!"id_tmp" %in% names(x)) {
            x$id_tmp <- 1:length(x)
        }
    }
    
    tile_name <- FALSE
    if (!"dem" %in% names(x)) {
        x$dem <- numeric(length(x))
        if (!"tile_name" %in% names(x)) {
            x$tile_name <- rep(NA_character_, length(x))
            tile_name <- TRUE
        } else {
            warning("Attribute column 'tile_name' has been updated!")
        }
        for (i in 1:length(spdf.tiles)) {
            id <- x[spdf.tiles[i,], ]$id_tmp
            x$tile_name[id] <- spdf.tiles$name[i]
            f <- paste0(hydflood_cache$cache_path_get(), "/", spdf.tiles$name[i],
                        "_DEM.tif")
            if (!file.exists(f)) {
                utils::download.file(spdf.tiles$url[i], f, quiet = TRUE)
            }
            x$dem[id] <- round(raster::extract(raster::raster(f), x[id, ]), 2)
            if (length(id) == length(x)) {
                break
            }
        }
    }
    
    if (! "csa" %in% names(x)) {
        csa_file <- paste0(hydflood_cache$cache_path_get(), "/spdf.active_floo",
                           "dplain_", tolower(river), "_csa.rda")
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
        
        x$csa <- rep(NA_integer_, length(x))
        spdf.csa <- spdf.active_floodplain_elbe_csa[x, ]
        for (i in 1:length(spdf.csa)) {
            id <- x[spdf.csa[i,], ]$id_tmp
            x$csa[id] <- spdf.csa[i,]$STATION_INT
            if (length(id) == length(x)) {
                break
            }
        }
    }
    
    #####
    # processing
    #####
    id_nna <- which(!is.na(x$csa))
    if ("flood3" %in% names(x)) {
        warning("Attribute column 'flood3' has been updated!")
    }
    x$flood3 <- rep(NA_integer_, length(x))
    x$flood3[id_nna] <- 0
    
    wldf_initial <- WaterLevelDataFrame(river = river, time = as.POSIXct(NA),
                                        station_int = sort(unique(x$csa[id_nna])))
    
    # loop over all time steps
    for (i in seq) {
        if (type_date) {
            time <- as.POSIXct(format(as.Date(i, as.Date("1970-01-01")),
                                      "%Y-%m-%d"), tz = "CET")
            wldf <- wldf_initial
            setTime(wldf) <- time
            wldf <- hyd1d::waterLevel(wldf)
        } else {
            time <- as.POSIXct(i, origin = "1970-01-01 00:00:00")
            wldf <- wldf_initial
            setTime(wldf) <- time
            wldf <- hyd1d::waterLevelPegelonline(wldf)
        }
        df <- as.data.frame(wldf)
        xdf <- merge(x@data, df, by.x = "csa", by.y = "station_int",
                     all.x = TRUE, incomparables = NA)
        id <- xdf$id_tmp[which(xdf$w > xdf$dem)]
        x$flood3[id] <- x$flood3[id] + 1
    }
    
    x$id_tmp <- NULL
    
    return(x)
}

