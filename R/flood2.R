#' @name flood2
#' @rdname flood2
#' 
#' @title Function to compute flood extent or flood duration along German 
#'   federal waterways Elbe and Rhine using the 1D water level algorythm
#'   \code{hyd1d::waterLevelFlood2()}
#' 
#' @description Computes flood extent, if \code{length(seq)} equal 1, or flood 
#'   duration for the active floodplains along German federal waterways Elbe 
#'   and Rhine based on 1D water levels computed by \code{waterLevelFlood2()} 
#'   provided by package \code{hyd1d}.
#' 
#' @param x has to by type \code{RasterStack} and has to include both input 
#'   RasterLayers \code{csa} (cross section areas) and \code{dem} (digital 
#'   elevation model). To compute water levels along the River Elbe \code{x} 
#'   has to be in the coordinate reference system 
#'   \href{http://spatialreference.org/ref/epsg/etrs89-utm-zone-33n/}{ETRS 1989 UTM 33N},
#'   for River Rhine in 
#'   \href{http://spatialreference.org/ref/epsg/etrs89-utm-zone-32n/}{ETRS 1989 UTM 32N}.
#'   Other coordinate reference systems are not permitted.
#' @param seq has to be type \code{c("POSIXct", "POSIXt")} or \code{Date} and 
#'   have length larger 0. Values of \code{seq} must be in the 
#'   temporal range between 1990-01-01 and yesterday (\code{Sys.Date() - 1}). 
#'   Internally \code{waterLevelFlood2()} uses \code{getGaugingDataW()} to 
#'   obtain daily water level information from \code{df.gauging_data}.
#' @param filename supplies an optional output filename and has to be type 
#'   \code{character}.
#' @param \dots additional arguments as for \code{\link[raster]{writeRaster}}.
#' 
#' @return Raster* object with flood duration in the range of 
#'   \code{[0, length(seq)]}.
#' 
#' @details For every time step provided in \code{seq} \code{flood2()} computes 
#'   a 1D water level using \code{waterLevelFlood2} along the requested river
#'   section. This 1D water level is transfered to a \code{wl} (water level) 
#'   RasterLayer, which is in fact a copy of the \code{csa} 
#'   (cross section areas) RasterLayer, and then compared to the \code{dem} 
#'   (digital elevation model) RasterLayer. Where the \code{wl} RasterLayer is
#'   higher than the \code{dem} RasterLayer flood duration is increased by 1.
#' 
#' @seealso \code{\link[hyd1d]{getGaugingDataW}},
#'   \code{\link[hyd1d]{waterLevelFlood2}}, 
#'   \code{\link[raster]{writeRaster}}, 
#'   \code{\link[raster]{rasterOptions}}
#' 
#' @references 
#'   \insertRef{bfn_auenzustandsbericht_2009}{hydflood}
#' 
#' @examples \dontrun{
#' library(hydflood)
#' 
#' # import the raster data and create a raster stack
#' crs <- crs("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")
#' x <- hydRasterStack(ext = c(309000, 310000, 5749000, 5750000), 
#'                     crs = crs)
#' 
#' # create a temporal sequence
#' seq <- seq(as.Date("2016-12-01"), as.Date("2016-12-31"), by = "day")
#' 
#' # compute a flood duration
#' fd <- flood2(x = x, seq = seq)
#' }
#' 
#' @export
#' 
flood2 <- function(x, seq, filename = '', ...) {
    
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
        if (class(x) != "hydRasterStack") {
            errors <- c(errors, paste0("Error ", l(errors), ": 'x' must be ",
                                       "type 'hydRasterStack'."))
        }
        # names
        if (!(all(names(x) == c("dem", "csa")))) {
            errors <- c(errors, paste0("Error ", l(errors), ": names(x) ",
                                       "must be c('dem', 'csa')."))
        }
        
        # crs
        crs_string <- raster::crs(x, asText = TRUE)
        etrs_1989_utm_32_string <- sp::CRS("+init=epsg:25832")
        etrs_1989_utm_33_string <- sp::CRS("+init=epsg:25833")
        
        if ( !(raster::compareCRS(crs_string, etrs_1989_utm_32_string)) & 
             !(raster::compareCRS(crs_string, etrs_1989_utm_33_string))) {
            errors <- c(errors, paste0("Error ", l(errors), ": The projection",
                                       " of x must be either 'ETRS 1989 UTM 32",
                                       "N' or 'ETRS 1989 UTM 33N'."))
        } else {
            if (raster::compareCRS(crs_string, etrs_1989_utm_32_string)) {
                zone <- "32"
                river <- "Rhein"
            } else if (raster::compareCRS(crs_string, 
                                          etrs_1989_utm_33_string)) {
                zone <- "33"
                river <- "Elbe"
            } else {
                stop(errors)
            }
        }
        
        # check position
        if (exists("river")) {
            # access the spdf.active_floodplain_* data
            active_floodplain <- paste0("spdf.active_floodplain_", 
                                        tolower(river))
            get(active_floodplain, pos = -1)
            if (river == "Elbe") {
                l.over <- sp::over(rasterextent2polygon(x), 
                                   spdf.active_floodplain_elbe,
                                   returnList = TRUE)
                if (! (length(unlist(l.over)) > 0)) {
                    errors <- c(errors, paste0("Error ", l(errors), ": x does ",
                                               "NOT overlap with the active fl",
                                               "oodplain of River Elbe."))
                }
            } else if (river == "Rhein") {
                l.over <- sp::over(rasterextent2polygon(x), 
                                   spdf.active_floodplain_rhein,
                                   returnList = TRUE)
                if (! (length(unlist(l.over)) > 0)) {
                    errors <- c(errors, paste0("Error ", l(errors), ": x does ",
                                               "NOT overlap with the active fl",
                                               "oodplain of River Rhine."))
                }
            }
        }
    }
    
    ## seq
    if (missing(seq)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The 'seq' ",
                                   "argument has to be supplied."))
    } else {
        # class
        if (! (all(class(seq) == "Date")) &
            ! (all(class(seq) == c("POSIXct", "POSIXt")))) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'seq' must be",
                                       " either type 'Date' or c('POSIXct', 'P",
                                       "OSIXt')."))
        }
        # length
        if (length(seq) < 1L) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'seq' must ha",
                                       "ve length larger 0."))
        }
        # NA and possible range
        if (any(is.na(seq))){
            errors <- c(errors, paste0("Error ", l(errors), ": 'seq' or elem",
                                       "ents of it must not be NA."))
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
                    errors <- c(errors, paste0("Error ", l(errors), ": Val",
                                               "ues of 'seq' must be betwe",
                                               "en 1990-01-01 and yesterda",
                                               "y."))
                }
                type_date <- TRUE
            }
        }
    }
    
    ## filename
    if (! missing(filename)) {
        if (class(filename) != "character") {
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
    csa <- x$csa
    dem <- x$dem
    
    # out template
    out <- raster::raster(csa)
    
    # describe out's data attributes
    attributes <- out@data@attributes
    attributes <- append(attributes, paste0("flood duration computed by hydflo",
                                            "od::flood2() for the following t",
                                            "emporal sequence with length ",
                                            length(seq), ":"))
    attributes <- append(attributes, seq)
    out@data@attributes <- attributes
    
    # water level template
    waterlevel <- raster::raster(dem)
    
    # initialize the WaterLevelDataFrame
    station_int <- as.integer(raster::unique(csa))
    wldf_initial <- hyd1d::WaterLevelDataFrame(river = river,
                                               time = as.POSIXct(NA),
                                               station_int = station_int)
    
    # check memory requirements
    big <- ! raster::canProcessInMemory(out, 4)
    filename <- raster::trim(filename)
    if (big & filename == '') {
        filename <- raster::rasterTmpFile()
    }
    if (filename != '') {
        out <- raster::writeStart(out, filename, ...)
        todisk <- TRUE
    } else {
        vv <- matrix(ncol = nrow(out), nrow = ncol(out))
        todisk <- FALSE
    }
    
    #####
    # processing
    ## 
    # compute all water levels through a loop over all time steps
    wldfs <- vector(mode = "list", length = length(seq))
    j <- 1
    for (i in seq) {
        if (type_date) {
            time <- as.POSIXct(format(as.Date(i, as.Date("1970-01-01")), 
                                      "%Y-%m-%d"), tz = "CET")
        } else {
            time <- as.POSIXct(i, origin = "1970-01-01 00:00:00")
        }
        wldf <- wldf_initial
        setTime(wldf) <- time
        wldfs[[j]] <- hyd1d::waterLevelFlood2(wldf)
        j <- j + 1
    }
    
    ##
    # raster processing
    bs <- raster::blockSize(csa)
    pb <- raster::pbCreate(bs$n, ...)
    
    if (todisk) {
        for (i in 1:bs$n) {
            # vectorize cross section areas (integer)
            v_csa <- raster::getValues(csa, row = bs$row[i], nrows = bs$nrows[i])
            # get unique stations for the csa subset
            v_stations <- stats::na.omit(unique(v_csa))
            # copy v_csa to v_fd to create a template results vector with the 
            # same size and type
            v_fd <- rep(0, length(v_csa))
            
            # vectorize digital elevation model (numeric)
            v_dem <- raster::getValues(dem, row = bs$row[i], nrows = bs$nrows[i])
            # copy v_dem to v_fwl to create a template vector with the same 
            # size and type
            v_wl <- rep(-999, length(v_csa))
            
            # handle NA's
            id_na <- is.na(v_csa) | is.na(v_dem) 
            id_nona <- !id_na
            
            # loop over all time steps
            for (j in 1:length(seq)) {
                # transfer the water level info to v_wl
                for (a_station in v_stations) {
                    v_wl[v_csa == a_station] <- 
                               wldfs[[j]]$w[wldfs[[j]]$station_int == a_station]
                }
                
                # compare the water level raster to the dem
                v_fd[id_nona][v_dem[id_nona] < v_wl[id_nona]] <- 
                    v_fd[id_nona][v_dem[id_nona] < v_wl[id_nona]] + 1
            }
            
            # transfer NA's
            v_fd[id_na] <- NA
            
            # write the resulting flood durations into out
            out <- raster::writeValues(out, v_fd, bs$row[i])
            raster::pbStep(pb, i)
        }
        out <- raster::writeStop(out)
    } else {
        for (i in 1:bs$n) {
            # vectorize cross section areas (integer)
            v_csa <- raster::getValues(csa, row = bs$row[i], nrows = bs$nrows[i])
            # get unique stations for the csa subset
            v_stations <- stats::na.omit(unique(v_csa))
            # copy v_csa to v_fd to create a template results vector with the 
            # same size and type
            v_fd <- rep(0, length(v_csa))
            
            # vectorize digital elevation model (numeric)
            v_dem <- raster::getValues(dem, row = bs$row[i], nrows = bs$nrows[i])
            # copy v_dem to v_fwl to create a template vector with the same 
            # size and type
            v_wl <- rep(-999, length(v_csa))
            
            # handle NA's
            id_na <- is.na(v_csa) | is.na(v_dem) 
            id_nona <- !id_na
            
            # loop over all time steps
            for (j in 1:length(seq)) {
                # transfer the water level info to v_wl
                for (a_station in v_stations) {
                    v_wl[v_csa == a_station] <- 
                        wldfs[[j]]$w[wldfs[[j]]$station_int == a_station]
                }
                
                # compare the water level raster to the dem
                v_fd[id_nona][v_dem[id_nona] < v_wl[id_nona]] <- 
                    v_fd[id_nona][v_dem[id_nona] < v_wl[id_nona]] + 1
            }
            
            # transfer NA's
            v_fd[id_na] <- NA
            
            cols <- bs$row[i]:(bs$row[i] + bs$nrows[i]-1)
            vv[,cols] <- matrix(v_fd, nrow = out@ncols)
            raster::pbStep(pb, i)
        }
        out <- raster::setValues(out, as.vector(vv))
    }
    raster::pbClose(pb)
    return(out)
}

