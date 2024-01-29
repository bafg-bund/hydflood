#' @name floodExtentToLine
#' @rdname floodExtentToLine
#' 
#' @title Function to convert flood extents \code{SpatRaster} to
#'   \code{LINESTRING}s
#' 
#' @description Convert flood extents of characteristic water levels computed by
#'   \code{\link{floodCharacteristicWaterlevel}} or other 
#'   \code{flood...()}-functions to spatial \code{LINESTRING}s.
#' 
#' @param x has to be type \code{SpatRaster} with a range of \code{[0, 1]}.
#' @param area_min optional, positive \code{numeric} or \code{integer} value. It
#'   is used to filter and remove small wetted areas with a size below this
#'   threshold. \code{area_min} may be of class \code{units}.
#' @param smooth_method optional \code{character} specifying the method to use
#'   for the smoothing of the resulting line. Possible methods are: `"chaikin"`,
#'   `"ksmooth"`, `"spline"`, and `"densify"`. Each method has one or more
#'   parameters specifying the amount of smoothing to perform. See details for
#'   descriptions in \code{\link[smoothr]{smooth}}.
#' @param ... additional arguments specifying the amount of smoothing, passed on
#'   to the specific smoothing function. See details for descriptions in 
#'   \code{\link[smoothr]{smooth}}.
#' 
#' @return \code{sf} object with \code{LINESTRING}s separating dry from wetted 
#'   areas.
#' 
#' @seealso \code{\link{floodCharacteristicWaterlevel}}, \code{\link{flood1}},
#'   \code{\link{flood2}}, \code{\link{flood3}}
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
#'   # convert the flood extent to lines separting dry from wetted areas
#'   l <- floodExtentToLine(fe, as_units(100, "m²"), "ksmooth",
#'                          smoothness = 10)
#'   
#'   # plot the products
#'   plot(fe)
#'   plot(st_geometry(l), add = TRUE)
#' }
#' 
#' @export
#' 
floodExtentToLine <- function(x, area_min = NULL,
                              smooth_method = c("chaikin", "ksmooth", "spline",
                                                "densify"), ...) {
    
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
        } else {
            if (terra::nlyr(x = x) > 1) {
                errors <- c(errors, paste0("Error ", l(errors), ": 'x' must ha",
                                           "ve only 1 layer."))
            }
            
            mm <- minmax(x)
            if (mm["min", 1] != 0) {
                errors <- c(errors, paste0("Error ", l(errors), ": 'x' must ha",
                                           "ve a minimum value of 0."))
            }
            if (mm["max", 1] != 1) {
                errors <- c(errors, paste0("Error ", l(errors), ": 'x' must ha",
                                           "ve a maximum value of 1."))
            }
            
            if (mm["min", 1] == mm["max", 1]) {
                if (mm["min", 1] == 0) {
                    message(paste0("The selected area is fully dry. There floo",
                                   "dExtentToLine returns NULL."))
                    return(NULL)
                }
                if (mm["min", 1] == 1) {
                    message(paste0("The selected area is fully flooded. There ",
                                   "floodExtentToLine returns NULL."))
                    return(NULL)
                }
            }
        }
    }
    
    if (l(errors) != "1") {
        stop(paste0(errors, collapse="\n  "))
    }
    
    ## area_min
    if (!is.null(area_min)) {
        stopifnot(inherits(area_min, "numeric") | 
                      inherits(area_min, "integer") | 
                      inherits(area_min, "units"))
        stopifnot(as.numeric(area_min) > 0)
    }
    
    ## smooth
    if (!is.null(smooth_method)) {
        stopifnot(smooth_method %in% c("chaikin", "ksmooth", "spline", "densify"))
    }
    
    ## ...
    dots <- list(...)
    if (length(dots) > 0) {
        if (smooth_method == "chaikin") {
            stopifnot(all(names(dots) %in% c("refinements")))
        } else if (smooth_method == "ksmooth") {
            stopifnot(all(names(dots) %in% c("smoothness", "bandwidth", "n",
                                             "max_distance")))
        } else if (smooth_method == "chaikin") {
            stopifnot(all(names(dots) %in% c("vertex_factor", "n")))
        } else {
            stopifnot(all(names(dots) %in% c("n", "max_distance")))
        }
    }
    
    #####
    # processing
    ##
    # convert raster to polygons
    p <- sf::st_as_sf(terra::as.polygons(x))
    
    # select only wet areas
    cats <- unique(x)[, 1]
    col <- names(x)
    p <- p[which(as.data.frame(p)[, col] == cats[2]), ]
    
    # convert MULTIPOLYGON to POLYGON
    p <- sf::st_cast(p$geometry, "POLYGON")
    
    # remove small polygons and islands
    if (!is.null(area_min)) {
        if (inherits(area_min, "units")) {
            p <- p[which(sf::st_area(p) >= area_min)]
        } else {
            p <- p[as.numeric(sf::st_area(p)) >= area_min]
        }
        p <- smoothr::fill_holes(p, threshold = area_min)
    }
    
    # convert the remaining polygons to LINESTRING
    l <- sf::st_cast(p, "LINESTRING")
    
    # remove lines on the extent
    l.ext <- rasterextent2polygon(x)
    l.ext <- sf::st_cast(l.ext, "LINESTRING")
    l <- sf::st_difference(l, l.ext)
    
    if (!is.null(smooth_method)) {
        l <- tryCatch({
                smoothr::smooth(l, method = smooth_method, ...)
            },
            error = function(e){
                print(e)
                warning("smoothing failed")
                return(l)
            },
            warning = function(w){
                print(w)
                warning("smoothing failed")
                return(l)}
        )
    }
    
    return(l)
}
