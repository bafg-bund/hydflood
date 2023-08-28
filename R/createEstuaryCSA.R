#' @name createEstuaryCSA
#' @rdname createEstuaryCSA
#' 
#' @title Compute cross section areas (CSA) along the estuary of the German
#'   Federal Waterway Elbe
#' 
#' @description Compute cross section areas (CSA) along the estuary of the German
#'   Federal Waterway Elbe
#' 
#' @details We need to add a detailled description what this function does ...
#' 
#' @param x boundary of the study area as \code{sfc_POLYGON} geometry,
#'   containing one single feature.
#' @param axis \code{sfc_LINESTRING} containing one single feature.
#' @param left \code{sfc_MULTILINESTRING} containing one single feature.
#' @param right \code{sfc_MULTILINESTRING} containing one single feature.
#' @param density pbject of type \code{numeric}; \code{density} (points per
#'   distance unit) of the sampling, possibly a vector of length equal to the
#'   number of features (otherwise recycled); \code{density} may be of class
#'   \code{units}.
#' @param gs \code{sfc_POINT} containing point feature with gauging stations.
#'
#' @return \code{sfc_POLYGON} or \code{sfc_MULTIPOLYGON} with created features
#'   (CSA) and three attribute variables \code{c("id", "station", 
#'   "station_int")}.
#' 
#' @examples \donttest{
#'   options("hydflood.datadir" = tempdir())
#'   library(hydflood)
#'   
#'   # load the required geometries
#'   x <- st_read("~/hydflood/data-raw", "estuary_area")
#'   axis <- st_read("~/hydflood/data-raw", "estuary_axis")
#'   left <- st_read("~/hydflood/data-raw", "estuary_left")
#'   right <- st_read("~/hydflood/data-raw", "estuary_right")
#'   construction_lines <- rbind(left, right)
#'   density <- 1/1000
#'   gs <- st_read("~/hydflood/data-raw", "estuary_gs")
#'   
#'   # execute function
#'   sf.res <- createEstuaryCSA(x, axis, left, right, density, gs)
#'   
#'   # plot
#'   plot(st_geometry(construction_lines), col = "blue")
#'   plot(st_geometry(x), lwd = 2, add = TRUE)
#'   plot(st_geometry(sf.res), add = TRUE, lwd = 0.5, col = "grey90")
#'   plot(st_geometry(axis), lwd = 2, col = "red", add = TRUE)
#'   plot(st_geometry(gs), col = "red", bg = "red", pch = 21, add = TRUE)
#'   plot(st_geometry(gs)[!st_intersects(x, gs, sparse = FALSE)],
#'        col = "blue", bg = "blue", pch = 21, add = TRUE)
#'   text(st_coordinates(gs)[,"X"], st_coordinates(gs)[,"Y"], gs$ga_station,
#'        cex = 0.6, pos = 4)
#' }
#' 
#' @export
#' 
createEstuaryCSA <- function(x, axis, left, right, density, gs) {
    
    # check properties of all objects
    # x
    stopifnot(inherits(x, "sf") | inherits(x, "sfc"))
    stopifnot(st_geometry_type(x) == "POLYGON" | 
                  st_geometry_type(x) == "MULTIPOLYGON")
    stopifnot(nrow(x) == 1)
    stopifnot(!is.null(st_crs(x)$units))
    stopifnot(st_crs(x)$units == "m")
    
    # axis
    stopifnot(inherits(axis, "sf") | inherits(axis, "sfc"))
    stopifnot(st_geometry_type(axis) == "LINESTRING" | 
                  st_geometry_type(axis) == "MULTILINESTRING")
    stopifnot(nrow(axis) == 1)
    stopifnot(!is.null(st_crs(axis)$units))
    stopifnot(st_crs(axis)$units == "m")
    stopifnot(st_crs(axis) == st_crs(x))
    stopifnot(sf::st_intersects(x, axis, sparse = FALSE))
    
    # left
    stopifnot(inherits(left, "sf") | inherits(left, "sfc"))
    stopifnot(st_geometry_type(left) == "LINESTRING" | 
                  st_geometry_type(left) == "MULTILINESTRING")
    stopifnot(nrow(left) == 1)
    stopifnot(!is.null(st_crs(left)$units))
    stopifnot(st_crs(left)$units == "m")
    stopifnot(st_crs(left) == st_crs(x))
    stopifnot(!sf::st_intersects(x, left, sparse = FALSE))
    
    # right
    stopifnot(inherits(right, "sf") | inherits(right, "sfc"))
    stopifnot(st_geometry_type(right) == "LINESTRING" | 
                  st_geometry_type(right) == "MULTILINESTRING")
    stopifnot(nrow(right) == 1)
    stopifnot(!is.null(st_crs(right)$units))
    stopifnot(st_crs(right)$units == "m")
    stopifnot(st_crs(right) == st_crs(x))
    stopifnot(!sf::st_intersects(x, right, sparse = FALSE))
    
    # density
    stopifnot(inherits(density, "numeric") | inherits(density, "units"))
    stopifnot(length(density) == 1)
    
    # gs
    stopifnot(inherits(gs, "sf") | inherits(gs, "sfc")) 
    stopifnot(st_geometry_type(gs) == "POINT")
    stopifnot(!is.null(st_crs(gs)$units))
    stopifnot(st_crs(gs)$units == "m")
    stopifnot(st_crs(gs) == st_crs(x))
    stopifnot(any(sf::st_intersects(x, gs, sparse = FALSE)))
    
    # check other spatial overlaps
    stopifnot(!sf::st_intersects(axis, left, sparse = FALSE))
    stopifnot(!sf::st_intersects(axis, right, sparse = FALSE))
    stopifnot(!sf::st_intersects(left, right, sparse = FALSE))
    
    # stationing
    geometry <- st_sfc(st_cast(st_line_sample(axis, density = density),
                               "POINT"))
    id <- 1:length(geometry)
    if (!inherits(density, "units")) {
        units(density) <- 1/st_crs(x)$ud_unit
    }
    km <- NULL
    density <- units::set_units(density, 1/km)
    station <- (id - 1) / as.numeric(density)
    station_int <- as.integer(station * 1000)
    
    stations <- st_as_sf(
        data.frame(geometry = geometry, id = id,
                   station = station, station_int = station_int))
    
    # snap stations to right and left
    n_right <- as.matrix(st_coordinates(st_nearest_points(stations, right)))
    n_left <- as.matrix(st_coordinates(st_nearest_points(stations, left)))
    
    # loop over (almost) all stations to construct the CSA
    for (id in stations$id) {
        
        # skip first and last loop
        if (id == min(stations$id) | id == max(stations$id)) {next}
        
        # axis
        if (exists("a2")) {
            a1 <- a2
        } else {
            a1 <- data.frame(
                x = (st_coordinates(stations$geometry[id - 1])[, "X"] +
                         st_coordinates(stations$geometry[id])[, "X"]) / 2,
                y = (st_coordinates(stations$geometry[id - 1])[, "Y"] +
                         st_coordinates(stations$geometry[id])[, "Y"]) / 2)
        }
        a2 <- data.frame(
            x = (st_coordinates(stations$geometry[id + 1])[, "X"] +
                     st_coordinates(stations$geometry[id])[, "X"]) / 2,
            y = (st_coordinates(stations$geometry[id + 1])[, "Y"] +
                     st_coordinates(stations$geometry[id])[, "Y"]) / 2)
        
        # left
        if (exists("l2")) {
            l1 <- l2
        } else {
            l1 <- data.frame(
                x = (n_left[max(which(n_left[, "L1"] == (id - 1))), "X"] +
                         n_left[max(which(n_left[, "L1"] == id)), "X"]) / 2,
                y = (n_left[max(which(n_left[, "L1"] == (id - 1))), "Y"] +
                        n_left[max(which(n_left[, "L1"] == id)), "Y"]) / 2)
        }
        l2 <- data.frame(
            x = (n_left[max(which(n_left[, "L1"] == (id + 1))), "X"] + 
                     n_left[max(which(n_left[, "L1"] == id)), "X"]) / 2,
            y = (n_left[max(which(n_left[, "L1"] == (id + 1))), "Y"] + 
                     n_left[max(which(n_left[, "L1"] == id)), "Y"]) / 2)
        
        # right
        if (exists("r2")) {
            r1 <- r2
        } else {
            r1 <- data.frame(
                x = (n_right[max(which(n_right[, "L1"] == (id - 1))), "X"] + 
                         n_right[max(which(n_right[, "L1"] == id)), "X"]) / 2,
                y = (n_right[max(which(n_right[, "L1"] == (id - 1))), "Y"] + 
                         n_right[max(which(n_right[, "L1"] == id)), "Y"]) / 2)
        }
        r2 <- data.frame(
            x = (n_right[max(which(n_right[, "L1"] == (id + 1))), "X"] + 
                     n_right[max(which(n_right[, "L1"] == id)), "X"]) / 2,
            y = (n_right[max(which(n_right[, "L1"] == (id + 1))), "Y"] + 
                     n_right[max(which(n_right[, "L1"] == id)), "Y"]) / 2)
        
        pol <- as.matrix(rbind(a1, l1, l2, a2, r2, r1, a1))
        colnames(pol) <- c("X", "Y")
        rownames(pol) <- as.character(1:nrow(pol))
        
        sf.pol <- st_as_sf(
            data.frame(geometry = st_sfc(st_sfc(st_polygon(list(pol)))),
                       id = id,
                       station = stations$station[id],
                       station_int = stations$station_int[id]),
            crs = st_crs(x))
        
        if (nrow(sf.pol) > 1) {browser()}
        
        if (exists("sf.res")) {
            sf.res <- rbind(sf.res, sf.pol)
        } else {
            sf.res <- sf.pol
        }
    }
    
    sf.res <- st_make_valid(sf.res)
    sf.res <- tryCatch(st_cast(sf.res, "POLYGON"), 
                       warning = function(warn){
                           return(st_cast(sf.res, "MULTIPOLYGON"))
                       },
                       error = function(err) {
                           message(warn)
                           return(NULL)
                       })
    
    # get the remains (without warning)
    warn <- getOption("warn")
    options(warn = -1)
    sf.x_remain <- st_sf(st_cast(st_difference(x, st_union(sf.res)), "POLYGON"))
    options(warn = warn)
    
    if (nrow(sf.x_remain) > 0) {
        # join stations spatially
        sf.x_remain <- st_join(sf.x_remain, stations)
        sf.x_remain <- sf.x_remain[, names(sf.res)]
        
        # add remains to sf.res
        sf.res <- rbind(sf.res, sf.x_remain)
    }
    
    # order sf.res
    sf.res <- sf.res[order(sf.res$id),]
    rownames(sf.res) <- as.character(1:nrow(sf.res))
    
    # clip sf.res by x
    options(warn = -1)
    sf.res <- st_intersection(sf.res, st_geometry(x))
    options(warn = warn)
    
    return(sf.res)
}

