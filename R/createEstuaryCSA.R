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
#'   x <- st_read("~/hydflood/data-raw/estuary/elbe", "x", quiet = TRUE)
#'   axis <- st_read("~/hydflood/data-raw/estuary/elbe", "axis", quiet = TRUE)
#'   left <- st_read("~/hydflood/data-raw/estuary/elbe", "left", quiet = TRUE)
#'   right <- st_read("~/hydflood/data-raw/estuary/elbe", "right", quiet = TRUE)
#'   construction_lines <- rbind(left, right)
#'   density <- 1/1000
#'   gs <- st_read("~/hydflood/data-raw/estuary/elbe", "gs", quiet = TRUE)
#'   
#'   # execute function in 'default' mode
#'   sf.res_def <- createEstuaryCSA(x, axis, left, right, density, gs,
#'                                  "default")
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
#'   
#'   # execute function in 'linesplit' mode
#'   sf.res_lsp <- createEstuaryCSA(x, axis, left, right, density, gs,
#'                                  "linesplit")
#'   
#'   # plot
#'   plot(st_geometry(construction_lines), col = "blue")
#'   plot(st_geometry(x), lwd = 2, add = TRUE)
#'   plot(st_geometry(sf.res_lsp), add = TRUE, lwd = 0.5, col = "grey90")
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
    
    warn <- getOption("warn")
    debug <- FALSE
    if (debug) {
        tempdir <- "data-raw/estuary/ems" #tmpDir()
        tempfile <- paste0(LETTERS[sample(1:26, 8)], collapse = "")
    }
    
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
        stopifnot(mode == "default" | mode == "lines" | mode == "linesplit")
    } else {
        mode <- "default"
    }
    
    # stationing
    options(warn = -1)
    axis <- st_intersection(x[, "geometry"], axis)
    options(warn = warn)
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
                   station = station, station_int = station_int),
        crs = st_crs(x))
    
    if (debug) {
        options(warn = -1)
        sf::st_write(stations, tempdir, paste0(tempfile, "_stations_", mode),
                     driver = "ESRI Shapefile", append = FALSE, quiet = TRUE)
        options(warn = warn)
    }
    
    # spatial subset of gs
    options(warn = -1)
    gs <- st_intersection(x[, "geometry"], gs)
    options(warn = warn)
    
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
                    x = (st_coordinates(stations[id - 1, "geometry"])[, "X"] +
                           st_coordinates(stations[id, "geometry"])[, "X"]) / 2,
                    y = (st_coordinates(stations[id - 1, "geometry"])[, "Y"] +
                           st_coordinates(stations[id, "geometry"])[, "Y"]) / 2)
            }
            a2 <- data.frame(
                x = (st_coordinates(stations[id + 1, "geometry"])[, "X"] +
                         st_coordinates(stations[id, "geometry"])[, "X"]) / 2,
                y = (st_coordinates(stations[id + 1, "geometry"])[, "Y"] +
                         st_coordinates(stations[id, "geometry"])[, "Y"]) / 2)
            
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
        
        if (debug) {
            options(warn = -1)
            sf::st_write(sf.res, tempdir, paste0(tempfile, "_sf.res_", mode),
                         driver = "ESRI Shapefile", append = FALSE,
                         quiet = TRUE)
            options(warn = warn)
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
        options(warn = -1)
        sf.x_remain <- st_sf(st_cast(st_difference(x, st_union(sf.res)),
                                     "POLYGON"))
        options(warn = warn)
        if (debug) {
            options(warn = -1)
            sf::st_write(sf.x_remain, tempdir,
                         paste0(tempfile, "_sf.x_remain_", mode),
                         driver = "ESRI Shapefile", append = FALSE,
                         quiet = TRUE)
            options(warn = warn)
        }
        
        if (nrow(sf.x_remain) > 0) {
            # join stations spatially
            sf.x_remain <- st_join(sf.x_remain[, "geometry"], stations)
            sf.x_remain <- sf.x_remain[, names(sf.res)]
            
            # add remains to sf.res
            sf.res <- rbind(sf.res, sf.x_remain)
        }
    } else if (mode == "linesplit") {
        
        # splitting points along the axis
        start_end <- stations[c(1, nrow(stations)), ]
        axis_sp <- st_collection_extract(
            st_split(axis, st_combine(st_buffer(start_end, 0.05))),
            type = "LINESTRING")
        axis_sp <- axis_sp[
            which(lengths(st_intersects(axis_sp,
                                        st_buffer(start_end, 0.1))) == 2),]
        stations_sp <- st_sfc(
            st_cast(st_line_sample(axis_sp, density = density), "POINT"))
        stations_sp <- st_as_sf(
            data.frame(geometry = stations_sp,
                       id = 1:length(stations_sp)),
            crs = st_crs(x))
        if (debug) {
            options(warn = -1)
            sf::st_write(stations_sp, tempdir,
                         paste0(tempfile, "_stations_sp_", mode),
                         driver = "ESRI Shapefile", append = FALSE,
                         quiet = TRUE)
            options(warn = warn)
        }
        
        # snap stations to right and left
        n_right <- as.matrix(st_coordinates(st_nearest_points(stations_sp,
                                                              right)))
        id_sel <- seq(2, nrow(n_right), by = 2)
        n_right <- n_right[id_sel,]
        n_left <- as.matrix(st_coordinates(st_nearest_points(stations_sp,
                                                             left)))
        n_left <- n_left[id_sel,]
        
        if (debug) {
            sf.right <- st_as_sf(as.data.frame(n_right),
                                 coords = c("X", "Y"), crs = st_crs(x))
            options(warn = -1)
            sf::st_write(sf.right, tempdir,
                         paste0(tempfile, "_stations_right_", mode),
                         driver = "ESRI Shapefile", append = FALSE,
                         quiet = TRUE)
            options(warn = warn)
            sf.left <- st_as_sf(as.data.frame(n_left),
                                coords = c("X", "Y"), crs = st_crs(x))
            options(warn = -1)
            sf::st_write(sf.left, tempdir,
                         paste0(tempfile, "_stations_left_", mode),
                         driver = "ESRI Shapefile", append = FALSE,
                         quiet = TRUE)
            options(warn = warn)
        }
        
        # loop over (almost) all stations to construct the CSA
        for (id in 1:nrow(stations_sp)) {
            
            # skip last loop
            if (id == nrow(stations_sp)) {break}
            
            # axis
            if (exists("a2")) {
                a1 <- a2
            } else {
                a1 <- data.frame(x = st_coordinates(stations_sp[id,])[, "X"],
                                 y = st_coordinates(stations_sp[id,])[, "Y"])
            }
            a2 <- data.frame(x = st_coordinates(stations_sp[id + 1,])[, "X"],
                             y = st_coordinates(stations_sp[id + 1,])[, "Y"])
            
            # left
            if (exists("l2")) {
                l1 <- l2
            } else {
                l1 <- data.frame(x = n_left[id, "X"], y = n_left[id, "Y"])
            }
            l2 <- data.frame(x = n_left[id + 1, "X"], y = n_left[id + 1, "Y"])
            
            # right
            if (exists("r2")) {
                r1 <- r2
            } else {
                r1 <- data.frame(x = n_right[id, "X"], y = n_right[id, "Y"])
            }
            r2 <- data.frame(x = n_right[id + 1, "X"], y = n_right[id + 1, "Y"])
            
            pol <- as.matrix(rbind(a1, l1, l2, a2, r2, r1, a1))
            colnames(pol) <- c("X", "Y")
            rownames(pol) <- as.character(1:nrow(pol))
            
            sf.pol <- st_as_sf(
                data.frame(geometry = st_sfc(st_polygon(list(pol))),
                           id = id + 1,
                           station = stations$station[id + 1],
                           station_int = stations$station_int[id + 1]),
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
        options(warn = -1)
        sf.x_remain <- st_sf(st_cast(st_difference(x, st_union(sf.res)),
                                     "POLYGON"))
        options(warn = warn)
        
        if (debug) {
            options(warn = -1)
            sf::st_write(sf.x_remain, tempdir,
                         paste0(tempfile, "_sf.x_remain_", mode),
                         driver = "ESRI Shapefile", append = FALSE,
                         quiet = TRUE)
            options(warn = warn)
        }
        
        if (nrow(sf.x_remain) > 0) {
            # join stations spatially
            sf.x_remain <- st_join(sf.x_remain[, "geometry"], stations)
            sf.x_remain <- sf.x_remain[, names(sf.res)]
            
            # check and fix unstationed, remaining parts of x
            id_na <- which(is.na(sf.x_remain$station))
            if (length(id_na) > 0) {
                
                # identify intersections between NA areas sf.res 
                id_res <- st_intersects(sf.x_remain[id_na,], sf.res)
                
                # select areas with only 1 intersection
                id_res_len <- unlist(lapply(id_res, length)) == 1
                
                if (any(id_res_len)) {
                    id_na_sel <- id_na[id_res_len]
                    id_res_sel <- unlist(id_res[id_res_len])
                    
                    # overwrite NA's
                    sf.x_remain$id[id_na_sel] <- sf.res$id[id_res_sel]
                    sf.x_remain$station[id_na_sel] <- sf.res$station[id_res_sel]
                    sf.x_remain$station_int[id_na_sel] <- 
                        sf.res$station_int[id_res_sel]
                    
                    # split union and no-union polygons
                    sf.union_remain <- sf.x_remain[id_na_sel, ]
                    sf.x_remain <- sf.x_remain[-id_na_sel, ]
                    sf.union <- sf.res[id_res_sel, ]
                    sf.res <- sf.res[-id_res_sel, ]
                    
                    # union NA polygons to stationed polygons
                    options(warn = -1)
                    for (id in 1:length(id_na_sel)) {
                        if (exists("sf.x_remains")) {
                            sf.x_remains_tm <- st_union(sf.union[id,],
                                                        sf.union_remain[id,])
                            sf.x_remains_tm <- sf.x_remains_tm[, names(sf.res)]
                            sf.x_remains <- rbind(sf.x_remains, sf.x_remains_tm)
                        } else {
                            sf.x_remains <- st_union(sf.union[id,],
                                                     sf.union_remain[id,])
                            sf.x_remains <- sf.x_remains[, names(sf.res)]
                        }
                    }
                    options(warn = warn)
                    
                    if (debug) {
                        options(warn = -1)
                        sf::st_write(sf.x_remains, tempdir,
                                     paste0(tempfile, "_sf.x_remains_", mode),
                                     driver = "ESRI Shapefile", append = FALSE,
                                     quiet = TRUE)
                        options(warn = warn)
                    }
                    
                    # add remains to sf.res
                    sf.res <- rbind(sf.res, sf.x_remain, sf.x_remains)
                }
            } else {
                # add remains to sf.res
                sf.res <- rbind(sf.res, sf.x_remain)
            }
        }
        
    } else if (mode == "lines") {
        
        # snap gs to the stations
        gs$id_stations <- st_nearest_feature(gs, stations)
        gs <- gs[order(gs$id_stations), ]
        stations_sel <- stations[gs$id_stations, ]
        
        # stations_inter
        
        
        if (debug) {
            options(warn = -1)
            sf::st_write(stations_sel, tempdir,
                         paste0(tempfile, "_stations_sel_", mode),
                         driver = "ESRI Shapefile", append = FALSE,
                         quiet = TRUE)
            options(warn = warn)
        }
        
        # construct csa centerpoints along left and right
        for (i in 1:(nrow(gs) - 1)) {
            if (debug) {
                print(paste0("gs: ", i))
            }
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
            if (all(n_do_left == n_up_left)) {
                if (debug) {
                    print(paste0("l = "))
                }
                p_left <- st_as_sf(n_do_left, coords = c("X", "Y"),
                                   crs = st_crs(x))
                p_left <- p_left[rep(1, n_seg + 1), ]
                p_left$id <- (stations_sel$id[i] + 1):stations_sel$id[i + 1]
                p_left$section <- i
                
                if (exists("stations_left")) {
                    stations_left <- rbind(stations_left, p_left)
                } else {
                    stations_left <- p_left
                }
            } else {
                if (debug) {
                    print(paste0("l <> "))
                }
                p_left <- st_as_sf(rbind(n_up_left, n_do_left),
                                   coords = c("X", "Y"), crs = st_crs(x))
                p_left$id <- c(stations_sel$id[i], stations_sel$id[i + 1])
                p_left$section <- i
                l_left <- st_collection_extract(
                    st_split(left, st_combine(st_buffer(p_left, 0.05))), 
                    type = "LINESTRING")
                id <- which(
                    lengths(st_intersects(l_left, st_buffer(p_left, 0.1))) == 2)
                if (length(id) != 1) {
                    print("left")
                    print(i)
                    id <- id[2]
                }
                l_left <- l_left[id, "geometry"]
                
                p_left_inter <- st_as_sf(
                    st_cast(
                        st_line_sample(
                            x = l_left, n = 2 * n_seg + 1,
                            sample = 1:(2 * n_seg + 1) / (2 * n_seg + 2)),
                    "POINT"))
                st_geometry(p_left_inter) <- "geometry"
                p_left_inter <- p_left_inter[seq(1, 2 * n_seg + 1, 2), ]
                p_left_inter$id <- (stations_sel$id[i] + 1):
                    (stations_sel$id[i + 1])
                p_left_inter$section <- i
                
                if (exists("stations_left")) {
                    stations_left <- rbind(stations_left, p_left_inter)
                } else {
                    stations_left <- p_left_inter
                }
            }
            
            # split right line
            if (all(n_do_right == n_up_right)) {
                if (debug) {
                    print(paste0("r = "))
                }
                p_right <- st_as_sf(n_do_right, coords = c("X", "Y"),
                                    crs = st_crs(x))
                p_right <- p_right[rep(1, n_seg + 1), ]
                p_right$id <- (stations_sel$id[i] + 1):stations_sel$id[i + 1]
                p_right$section <- i
                
                if (exists("stations_right")) {
                    stations_right <- rbind(stations_right, p_right)
                } else {
                    stations_right <- p_right
                }
            } else {
                if (debug) {
                    print(paste0("r <> "))
                }
                p_right <- st_as_sf(rbind(n_up_right, n_do_right),
                                    coords = c("X", "Y"), crs = st_crs(x))
                p_right$id <- c(stations_sel$id[i], stations_sel$id[i + 1])
                p_right$section <- i
                l_right <- st_collection_extract(
                    st_split(right, st_combine(st_buffer(p_right, 0.05))), 
                    type = "LINESTRING")
                id <- which(
                    lengths(
                        st_intersects(l_right, st_buffer(p_right, 0.1))) == 2)
                if (length(id) != 1) {
                    browser()
                    print("right")
                    print(i)
                    id <- id[2]
                }
                l_right <- l_right[id, "geometry"]
                
                p_right_inter <- st_as_sf(
                    st_cast(
                        st_line_sample(
                            x = l_right, n = 2 * n_seg + 1,
                            sample = 1:(2 * n_seg + 1) / (2 * n_seg + 2)),
                        "POINT"))
                st_geometry(p_right_inter) <- "geometry"
                p_right_inter <- p_right_inter[seq(1, 2 * n_seg + 1, 2), ]
                p_right_inter$id <- (stations_sel$id[i] + 1):
                    (stations_sel$id[i + 1])
                p_right_inter$section <- i
                
                if (exists("stations_right")) {
                    stations_right <- rbind(stations_right, p_right_inter)
                } else {
                    stations_right <- p_right_inter
                }
            }
        }
        
        stations_left <- stations_left[order(stations_left$id), ]
        stations_right <- stations_right[order(stations_right$id), ]
        stations_left$station <- stations$station[stations_left$id]
        stations_left$station_int <- stations$station_int[stations_left$id]
        stations_right$station <- stations$station[stations_right$id]
        stations_right$station_int <- stations$station_int[stations_right$id]
        row.names(stations_left) <- 1:nrow(stations_left)
        row.names(stations_right) <- 1:nrow(stations_right)
        
        if (debug) {
            options(warn = -1)
            sf::st_write(stations_right, tempdir,
                         paste0(tempfile, "_stations_right_", mode),
                         driver = "ESRI Shapefile", append = FALSE,
                         quiet = TRUE)
            sf::st_write(stations_left, tempdir,
                         paste0(tempfile, "_stations_left_", mode),
                         driver = "ESRI Shapefile", append = FALSE,
                         quiet = TRUE)
            options(warn = warn)
        }
        
        # loop over (almost) all stations to construct the CSA
        for (i in 1:(nrow(stations_left) - 1)) {
            if (debug) {
                print(paste0("cs: ", i))
            }
            
            # id
            id_s <- which(stations$id == stations_left$id[i])
            if (debug) {
                print(id_s)
            }
            
            # axis
            if (exists("a2")) {
                a1 <- a2
            } else {
                a1 <- data.frame(
                    X = (st_coordinates(stations[id_s - 1, "geometry"])[, "X"] +
                             st_coordinates(stations[id_s, "geometry"])[, "X"]) / 2,
                    Y = (st_coordinates(stations[id_s - 1, "geometry"])[, "Y"] +
                             st_coordinates(stations[id_s, "geometry"])[, "Y"]) / 2)
            }
            a2 <- data.frame(
                X = (st_coordinates(stations[id_s, "geometry"])[, "X"] +
                         st_coordinates(stations[id_s + 1, "geometry"])[, "X"]) / 2,
                Y = (st_coordinates(stations[id_s, "geometry"])[, "Y"] +
                         st_coordinates(stations[id_s + 1, "geometry"])[, "Y"]) / 2)
            
            # left
            if (exists("l2")) {
                l1 <- l2
            } else {
                l1 <- as.data.frame(st_coordinates(stations_left[i, ]))
            }
            l2 <- as.data.frame(st_coordinates(stations_left[i + 1, ]))
            
            # right
            if (exists("r2")) {
                r1 <- r2
            } else {
                r1 <- as.data.frame(st_coordinates(stations_right[i, ]))
            }
            r2 <- as.data.frame(st_coordinates(stations_right[i + 1, ]))
            
            pol <- as.matrix(rbind(a1, l1, l2, a2, r2, r1, a1))
            rownames(pol) <- as.character(1:nrow(pol))
            
            sf.pol <- st_as_sf(
                data.frame(geometry = st_sfc(st_polygon(list(pol))),
                           id = id_s,
                           station = stations$station[id_s],
                           station_int = stations$station_int[id_s],
                           section = stations_left$section[i]),
                crs = st_crs(x))
            
            if (!sf::st_is_valid(sf.pol)) {
                warning(paste0("The geometry of CSA ", i, " is not valid!\n",
                               "  Therefore it is skipped."))
                next
            }
            
            if (nrow(sf.pol) > 1) {browser()}
            
            if (exists("sf.res")) {
                sf.res <- rbind(sf.res, sf.pol)
            } else {
                sf.res <- sf.pol
            }
        }
        
        if (debug) {
            options(warn = -1)
            sf::st_write(sf.res, tempdir, paste0(tempfile, "_sf.res_", mode),
                         driver = "ESRI Shapefile", append = FALSE,
                         quiet = TRUE)
            options(warn = warn)
        }
        
        options(warn = -1)
        sf.res_remain <- st_cast(st_difference(x, st_union(sf.res)), "POLYGON")
        options(warn = warn)
        
        if (debug) {
            options(warn = -1)
            sf::st_write(sf.res_remain, tempdir,
                         paste0(tempfile, "_sf.res_remain_", mode),
                         driver = "ESRI Shapefile", append = FALSE,
                         quiet = TRUE)
            options(warn = warn)
        }
        
        # add missing start
        sf.start <- sf.res_remain[stations[1,],]
        if ("FID" %in% names(sf.start)) {sf.start$FID <- NULL}
        sf.start$id <- numeric(length = nrow(sf.start))
        if (nrow(sf.start) > 0) {
            sf.start$id <- 1
        }
        
        sf.start$station <- numeric(length = nrow(sf.start))
        if (nrow(sf.start) > 0) {
            sf.start$station <- stations$station[
                which(stations$id == stations_left$id[1]) - 1]
        }
        sf.start$station_int <- numeric(length = nrow(sf.start))
        if (nrow(sf.start) > 0) {
            sf.start$station_int <- stations$station_int[
                which(stations$id == stations_left$id[1]) - 1]
        }
        sf.start$section <- numeric(length = nrow(sf.start))
        if (nrow(sf.start) > 0) {
            sf.start$section <- 0
        }
        sf.start <- sf.start[, c("id", "station", "station_int", "section",
                                 "geometry")]
        
        # add missing end
        stations_end <- stations[id_s + 1,]
        sf.end <- sf.res_remain[stations_end,]
        if ("FID" %in% names(sf.end)) {sf.end$FID <- NULL}
        sf.end$id <- numeric(length = nrow(sf.end))
        if (nrow(sf.end) > 0) {
            sf.end$id <- stations_end$id
        }
        sf.end$station <- numeric(length = nrow(sf.end))
        if (nrow(sf.end) > 0) {
            sf.end$station <- stations_end$station
        }
        sf.end$station_int <- numeric(length = nrow(sf.end))
        if (nrow(sf.end) > 0) {
            sf.end$station_int <- stations_end$station_int
        }
        sf.end$section <- numeric(length = nrow(sf.end))
        if (nrow(sf.end) > 0) {
            sf.end$section <- nrow(gs) - 1
        }
        sf.end <- sf.end[, c("id", "station", "station_int", "section",
                             "geometry")]
        
        # assemble final sf.res
        sf.res <- rbind(sf.start, sf.res, sf.end)
    } else {
        browser()
    }
    
    if (debug) {
        options(warn = -1)
        sf::st_write(sf.res, tempdir,
                     paste0(tempfile, "_sf.res_", mode),
                     driver = "ESRI Shapefile", append = FALSE, quiet = TRUE)
        options(warn = warn)
    }
    
    # order sf.res
    sf.res <- sf.res[order(sf.res$id),]
    rownames(sf.res) <- as.character(1:nrow(sf.res))
    
    # clip sf.res by x
    options(warn = -1)
    sf.res <- st_intersection(sf.res, st_geometry(x))
    options(warn = warn)
    
    # edit warning
    if (any(is.na(sf.res$station))) {
        warning(paste0("Some parts of 'x' have not been stationed. Therefore, ",
                       "the\n  resulting product needs careful manual editing",
                       "!"))
    }
    
    return(sf.res)
}

