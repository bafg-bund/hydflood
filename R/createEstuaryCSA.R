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
#' @param x boundary of the study area as \code{sfc_POLYGON} or
#'   \code{sfc_MULTIPOLYGON} geometry, containing one single feature.
#' @param axis \code{sfc_LINESTRING} or \code{sfc_MULTILINESTRING} containing
#'   one single feature. The direction of digitization determines the direction
#'   of stationing along this \code{axis}.
#' @param left \code{sfc_LINESTRING} or \code{sfc_MULTILINESTRING} containing
#'   one single feature.
#' @param right \code{sfc_LINESTRING} or \code{sfc_MULTILINESTRING} containing
#'   one single feature.
#' @param density object of type \code{numeric}; \code{density} (points per
#'   distance unit) of the sampling, possibly a vector of length equal to the
#'   number of features (otherwise recycled); \code{density} may be of class
#'   \code{units}.
#' @param gs \code{sfc_POINT} containing point feature with gauging stations.
#' @param mode \code{character} to set the construction mode, either 
#'   \code{default} or \code{lines}. For more information see \bold{details}.
#' 
#' @return \code{sfc_POLYGON} or \code{sfc_MULTIPOLYGON} with created features
#'   (CSA) and three attribute variables \code{c("id", "station", 
#'   "station_int")} in the \code{"default"} mode or even four attributes
#'   \code{c("id", "station", "station_int", "section")} in the \code{"lines"}
#'   mode.
#' 
#' @examples \donttest{
#'   options("hydflood.datadir" = tempdir())
#'   library(hydflood)
#'   
#'   # load the required geometries
#'   x <- st_read("~/hydflood/data-raw", "estuary_area", quiet = TRUE)
#'   axis <- st_read("~/hydflood/data-raw", "estuary_axis", quiet = TRUE)
#'   left <- st_read("~/hydflood/data-raw", "estuary_left", quiet = TRUE)
#'   right <- st_read("~/hydflood/data-raw", "estuary_right", quiet = TRUE)
#'   construction_lines <- rbind(left, right)
#'   density <- 1/1000
#'   gs <- st_read("~/hydflood/data-raw", "estuary_gs", quiet = TRUE)
#'   
#'   # execute function in 'default' mode
#'   sf.res_def <- createEstuaryCSA(x, axis, left, right, density, gs, "default")
#'   
#'   # plot
#'   plot(st_geometry(construction_lines), col = "blue")
#'   plot(st_geometry(x), lwd = 2, add = TRUE)
#'   plot(st_geometry(sf.res_def), add = TRUE, lwd = 0.5, col = "grey90")
#'   plot(st_geometry(axis), lwd = 2, col = "red", add = TRUE)
#'   plot(st_geometry(gs), col = "red", bg = "red", pch = 21, add = TRUE)
#'   plot(st_geometry(gs)[!st_intersects(x, gs, sparse = FALSE)],
#'        col = "blue", bg = "blue", pch = 21, add = TRUE)
#'   text(st_coordinates(gs)[,"X"], st_coordinates(gs)[,"Y"], gs$ga_station,
#'        cex = 0.6, pos = 4)
#'   
#'   # execute function in 'lines' mode
#'   sf.res_lin <- createEstuaryCSA(x, axis, left, right, density, gs, "lines")
#'   
#'   # plot
#'   plot(st_geometry(construction_lines), col = "blue")
#'   plot(st_geometry(x), lwd = 2, add = TRUE)
#'   plot(st_geometry(sf.res_lin), add = TRUE, lwd = 0.5, col = "grey90")
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
createEstuaryCSA <- function(x, axis, left, right, density, gs,
                             mode = NULL) {
    
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
    
    # mode
    if (!is.null(mode)) {
        stopifnot(inherits(mode, "character"))
        stopifnot(length(mode) == 1)
        stopifnot(mode == "default" | mode == "lines")
    } else {
        mode <- "default"
    }
    
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
    
    #####
    # processing modes
    if (mode == "default") {
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
                             n_right[max(which(n_right[, "L1"] == id)), "X"])/2,
                    y = (n_right[max(which(n_right[, "L1"] == (id - 1))), "Y"] +
                             n_right[max(which(n_right[, "L1"] == id)), "Y"])/2)
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
                data.frame(geometry = st_sfc(st_polygon(list(pol))),
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
        sf.x_remain <- st_sf(st_cast(st_difference(x, st_union(sf.res)),
                                     "POLYGON"))
        options(warn = warn)
        
        if (nrow(sf.x_remain) > 0) {
            # join stations spatially
            sf.x_remain <- st_join(sf.x_remain, stations)
            sf.x_remain <- sf.x_remain[, names(sf.res)]
            
            # add remains to sf.res
            sf.res <- rbind(sf.res, sf.x_remain)
        }
    } else {
        
        # snap gs to the stations
        gs$id_stations <- st_nearest_feature(gs, stations)
        stations_sel <- stations[gs$id_stations, ]
        
        # construct csa centerpoints along left and right
        for (i in 1:(nrow(gs) - 1)) {
            # upstream
            # snap stations_sel to right and left
            n_up_right <- as.data.frame(st_coordinates(
                    st_nearest_points(stations_sel[i,], right)))[2, 1:2]
            n_up_left <- as.data.frame(st_coordinates(
                    st_nearest_points(stations_sel[i,], left)))[2, 1:2]
            
            # downstream
            n_do_right <- as.data.frame(st_coordinates(
                    st_nearest_points(stations_sel[i + 1,], right)))[2, 1:2]
            n_do_left <- as.data.frame(st_coordinates(
                    st_nearest_points(stations_sel[i + 1,], left)))[2, 1:2]
            
            # number of segments
            n_seg <- stations_sel$id[i + 1] - stations_sel$id[i] - 1
            
            # split left line
            p_left <- st_as_sf(rbind(n_up_left, n_do_left),
                               coords = c("X", "Y"), crs = st_crs(x))
            p_left$id <- c(stations_sel$id[i], stations_sel$id[i + 1])
            p_left$section <- i
            l_left <- st_collection_extract(
                st_split(left, st_combine(st_buffer(p_left, 0.05))), 
                type = "LINESTRING")
            id <- which(lengths(st_intersects(l_left, st_buffer(p_left, 0.1)))
                        == 2)
            if (length(id) != 1) {browser()}
            l_left <- l_left$geometry[id]
            
            p_left_inter <- st_as_sf(st_cast(
                st_line_sample(l_left, n_seg, sample = 1:n_seg / (n_seg + 1)),
                "POINT"))
            st_geometry(p_left_inter) <- "geometry"
            p_left_inter$id <- (stations_sel$id[i] + 1):(stations_sel$id[i + 1]
                                                         - 1)
            p_left_inter$section <- i
            
            if (exists("stations_left")) {
                stations_left <- rbind(stations_left, p_left[2, ], p_left_inter)
            } else {
                stations_left <- rbind(p_left, p_left_inter)
            }
            
            # split right line
            p_right <- st_as_sf(rbind(n_up_right, n_do_right),
                                coords = c("X", "Y"), crs = st_crs(x))
            p_right$id <- c(stations_sel$id[i], stations_sel$id[i + 1])
            p_right$section <- i
            l_right <- st_collection_extract(
                st_split(right, st_combine(st_buffer(p_right, 0.05))), 
                type = "LINESTRING")
            id <- which(lengths(st_intersects(l_right, st_buffer(p_right, 0.1)))
                        == 2)
            if (length(id) != 1) {browser()}
            l_right <- l_right$geometry[id]
            
            p_right_inter <- st_as_sf(st_cast(
                st_line_sample(l_right, n_seg, sample = 1:n_seg / (n_seg + 1)),
                "POINT"))
            st_geometry(p_right_inter) <- "geometry"
            p_right_inter$id <- p_left_inter$id
            p_right_inter$section <- i
            
            if (exists("stations_right")) {
                stations_right <- rbind(stations_right, p_right[2, ],
                                        p_right_inter)
            } else {
                stations_right <- rbind(p_right, p_right_inter)
            }
        }
        
        stations_left <- stations_left[order(stations_left$id), ]
        stations_right <- stations_right[order(stations_right$id), ]
        stations_left$station <- stations$station[stations_left$id]
        stations_left$station_int <- stations$station_int[stations_left$id]
        stations_right$station <- stations$station[stations_right$id]
        stations_right$station_int <- stations$station_int[stations_right$id]
        
        # loop over (almost) all stations to construct the CSA
        for (i in 2:(nrow(stations_left) - 1)) {
            
            # id
            id <- stations_left$id[i]
            id_s <- which(stations$id == stations_left$id[i])
            
            # axis
            if (exists("a2")) {
                a1 <- a2
            } else {
                a1 <- data.frame(
                    x = (st_coordinates(stations$geometry[id_s - 1])[, "X"] +
                             st_coordinates(stations$geometry[id_s])[, "X"]) / 2,
                    y = (st_coordinates(stations$geometry[id_s - 1])[, "Y"] +
                             st_coordinates(stations$geometry[id_s])[, "Y"]) / 2)
            }
            a2 <- data.frame(
                x = (st_coordinates(stations$geometry[id_s])[, "X"] +
                         st_coordinates(stations$geometry[id_s + 1])[, "X"]) / 2,
                y = (st_coordinates(stations$geometry[id_s])[, "Y"] +
                         st_coordinates(stations$geometry[id_s + 1])[, "Y"]) / 2)
            
            # left
            if (exists("l2")) {
                l1 <- l2
            } else {
                l1 <- data.frame(
                    x = (st_coordinates(stations_left$geometry[id - 1])[, "X"] +
                             st_coordinates(stations_left$geometry[id])[, "X"]) / 2,
                    y = (st_coordinates(stations_left$geometry[id - 1])[, "Y"] +
                             st_coordinates(stations_left$geometry[id])[, "Y"]) / 2)
            }
            l2 <- data.frame(
                x = (st_coordinates(stations_left$geometry[id])[, "X"] +
                         st_coordinates(stations_left$geometry[id + 1])[, "X"]) / 2,
                y = (st_coordinates(stations_left$geometry[id])[, "Y"] +
                         st_coordinates(stations_left$geometry[id + 1])[, "Y"]) / 2)
            
            # right
            if (exists("r2")) {
                r1 <- r2
            } else {
                r1 <- data.frame(
                    x = (st_coordinates(stations_right$geometry[id - 1])[, "X"] +
                             st_coordinates(stations_right$geometry[id])[, "X"]) / 2,
                    y = (st_coordinates(stations_right$geometry[id - 1])[, "Y"] +
                             st_coordinates(stations_right$geometry[id])[, "Y"]) / 2)
            }
            r2 <- data.frame(
                x = (st_coordinates(stations_right$geometry[id])[, "X"] +
                         st_coordinates(stations_right$geometry[id + 1])[, "X"]) / 2,
                y = (st_coordinates(stations_right$geometry[id])[, "Y"] +
                         st_coordinates(stations_right$geometry[id + 1])[, "Y"]) / 2)
            
            pol <- as.matrix(rbind(a1, l1, l2, a2, r2, r1, a1))
            colnames(pol) <- c("X", "Y")
            rownames(pol) <- as.character(1:nrow(pol))
            
            sf.pol <- st_as_sf(
                data.frame(geometry = st_sfc(st_polygon(list(pol))),
                           id = id,
                           station = stations$station[id_s],
                           station_int = stations$station_int[id_s],
                           section = stations_left$section[id]),
                crs = st_crs(x))
            
            if (nrow(sf.pol) > 1) {browser()}
            
            if (exists("sf.res")) {
                sf.res <- rbind(sf.res, sf.pol)
            } else {
                sf.res <- sf.pol
            }
        }
        
        warn <- getOption("warn")
        options(warn = -1)
        sf.res_remain <- st_cast(st_difference(x, st_union(sf.res)), "POLYGON")
        options(warn = warn)
        
        # add missing start
        sf.start <- sf.res_remain[stations[1,],]
        if ("FID" %in% names(sf.start)) {sf.start$FID <- NULL}
        sf.start$id <- stations$id[1]
        sf.start$station <- stations$station[1]
        sf.start$station_int <- stations$station_int[1]
        sf.start$section <- 1
        
        # add missing end
        sf.end <- sf.res_remain[stations[id_s + 1,],]
        if ("FID" %in% names(sf.end)) {sf.end$FID <- NULL}
        sf.end$id <- stations$id[id_s + 1]
        sf.end$station <- stations$station[id_s + 1]
        sf.end$station_int <- stations$station_int[id_s + 1]
        sf.end$section <- nrow(gs) - 1
        
        # assemble final sf.res
        sf.res <- rbind(sf.start, sf.res, sf.end)
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

