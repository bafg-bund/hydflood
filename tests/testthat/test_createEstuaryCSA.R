library(hydflood)

context("createEstuaryCSA")

test_that("createEstuaryCSA: checks", {
    if (Sys.info()["nodename"] == "pvil-r") {
        x <- st_read("~/hydflood/data-raw/estuary/elbe", "x", quiet = TRUE)
        axis <- st_read("~/hydflood/data-raw/estuary/elbe", "axis", quiet = TRUE)
        left <- st_read("~/hydflood/data-raw/estuary/elbe", "left", quiet = TRUE)
        right <- st_read("~/hydflood/data-raw/estuary/elbe", "right", quiet = TRUE)
        density <- 1/1000
        gs <- st_read("~/hydflood/data-raw/estuary/elbe", "gs", quiet = TRUE)
        
        # check x
        expect_error(createEstuaryCSA(x = 1, axis, left, right, density, gs),
                     'inherits(x, "sf") | inherits(x, "sfc")', fixed = TRUE)
        expect_error(createEstuaryCSA(x = axis, axis, left, right, density, gs),
                     'st_geometry_type(x) == "POLYGON"', fixed = TRUE)
        expect_error(createEstuaryCSA(x = rbind(x, x), axis, left, right,
                                      density, gs),
                     'nrow(x) == 1', fixed = TRUE)
        expect_error(createEstuaryCSA(x = st_transform(x, "EPSG:4326"), axis,
                                      left, right, density, gs),
                     '!is.null(st_crs(x)$units)', fixed = TRUE)
        
        # check axis
        expect_error(createEstuaryCSA(x, axis = 1, left, right, density, gs),
                     'inherits(axis, "sf") | inherits(axis, "sfc")',
                     fixed = TRUE)
        expect_error(createEstuaryCSA(x, axis = x, left, right, density, gs),
                     'st_geometry_type(axis) == "LINESTRING"', fixed = TRUE)
        expect_error(createEstuaryCSA(x, axis = rbind(axis, axis), left, right,
                                      density, gs),
                     'nrow(axis) == 1', fixed = TRUE)
        expect_error(createEstuaryCSA(x, axis = st_transform(axis, "EPSG:4326"),
                                      left, right, density, gs),
                     '!is.null(st_crs(axis)$units)', fixed = TRUE)
        expect_error(createEstuaryCSA(x,
                                      axis = st_transform(axis, "EPSG:25833"),
                                      left, right, density, gs),
                     'st_crs(axis) == st_crs(x)', fixed = TRUE)
        
        m.axis_shift <- st_coordinates(axis)[, c("X", "Y")]
        m.axis_shift[,"Y"] <- m.axis_shift[,"Y"] + 100000
        axis_shift <- st_sf(id = 1, st_sfc(st_linestring(m.axis_shift, 
                                                         dim = "XY")),
                            crs = "EPSG:25832")
        expect_error(createEstuaryCSA(x, axis = axis_shift, left, right,
                                      density, gs),
                     'sf::st_intersects(x, axis, sparse = FALSE)', fixed = TRUE)
        
        # check left
        expect_error(createEstuaryCSA(x, axis, left = 1, right, density, gs),
                     'inherits(left, "sf") | inherits(left, "sfc")',
                     fixed = TRUE)
        expect_error(createEstuaryCSA(x, axis, left = x, right, density, gs),
                     'st_geometry_type(left) == "LINESTRING"', fixed = TRUE)
        expect_error(createEstuaryCSA(x, axis, left = rbind(left, left), right,
                                      density, gs),
                     'nrow(left) == 1', fixed = TRUE)
        expect_error(createEstuaryCSA(x, axis, 
                                      left = st_transform(left, "EPSG:4326"),
                                      right, density, gs),
                     '!is.null(st_crs(left)$units)', fixed = TRUE)
        expect_error(createEstuaryCSA(x, axis,
                                      left = st_transform(left, "EPSG:25833"),
                                      right, density, gs),
                     'st_crs(left) == st_crs(x)', fixed = TRUE)
        
        m.left_shift <- st_coordinates(left)[, c("X", "Y")]
        m.left_shift[,"Y"] <- m.left_shift[,"Y"] + 5000
        left_shift <- st_sf(id = 1, st_sfc(st_linestring(m.left_shift,
                                                         dim = "XY")),
                            crs = "EPSG:25832")
        expect_error(createEstuaryCSA(x, axis, left = left_shift, right,
                                      density, gs),
                     '!sf::st_intersects(x, left, sparse = FALSE)',
                     fixed = TRUE)
        
        # check right
        expect_error(createEstuaryCSA(x, axis, left, right = 1, density, gs),
                     'inherits(right, "sf") | inherits(right, "sfc")',
                     fixed = TRUE)
        expect_error(createEstuaryCSA(x, axis, left, right = x, density, gs),
                     'st_geometry_type(right) == "LINESTRING"', fixed = TRUE)
        expect_error(createEstuaryCSA(x, axis, left,
                                      right = rbind(right, right),
                                      density, gs),
                     'nrow(right) == 1', fixed = TRUE)
        expect_error(createEstuaryCSA(x, axis, left,
                                      right = st_transform(right, "EPSG:4326"),
                                      density, gs),
                     '!is.null(st_crs(right)$units)', fixed = TRUE)
        expect_error(createEstuaryCSA(x, axis, left,
                                      right = st_transform(right, "EPSG:25833"),
                                      density, gs),
                     'st_crs(right) == st_crs(x)', fixed = TRUE)
        
        m.right_shift <- st_coordinates(right)[, c("X", "Y")]
        m.right_shift[,"Y"] <- m.right_shift[,"Y"] - 5000
        right_shift <- st_sf(id = 1, st_sfc(st_linestring(m.right_shift,
                                                          dim = "XY")),
                            crs = "EPSG:25832")
        expect_error(createEstuaryCSA(x, axis, left, right = right_shift,
                                      density, gs),
                     '!sf::st_intersects(x, right, sparse = FALSE)',
                     fixed = TRUE)
        
        # density
        expect_error(createEstuaryCSA(x, axis, left, right, density = "a", gs),
                     'inherits(density, "numeric") | inherits(density, "units"',
                     fixed = TRUE)
        expect_no_error(createEstuaryCSA(x, axis, left, right,
                                         density = units::set_units(10, 1/km),
                                         gs, mode = "lines"))
        expect_error(createEstuaryCSA(x, axis, left, right, density = c(1, 2),
                                      gs),
                     'length(density) == 1', fixed = TRUE)
        
        # gs
        expect_error(createEstuaryCSA(x, axis, left, right, density, gs = 1),
                     'inherits(gs, "sf") | inherits(gs, "sfc")', fixed = TRUE)
        expect_error(createEstuaryCSA(x, axis, left, right, density, gs = x),
                     'st_geometry_type(gs) == "POINT"', fixed = TRUE)
        expect_error(createEstuaryCSA(x, axis, left, right, density,
                                      gs = st_transform(gs, "EPSG:4326")),
                     '!is.null(st_crs(gs)$units)', fixed = TRUE)
        expect_error(createEstuaryCSA(x, axis, left, right, density,
                                      gs = st_transform(gs, "EPSG:25833")),
                     'st_crs(gs) == st_crs(x)', fixed = TRUE)
        
        m.gs_shift <- st_coordinates(gs)[, c("X", "Y")]
        m.gs_shift[1, "Y"] <- m.gs_shift[1, "Y"] - 500000
        gs_shift <- st_sf(id = 1,
                          st_sfc(st_point(m.gs_shift[1, ], dim = "XY")),
                          crs = "EPSG:25832")
        expect_error(createEstuaryCSA(x, axis, left, right, density, gs_shift),
                     'any(sf::st_intersects(x, gs, sparse = FALSE))',
                     fixed = TRUE)
        
        # mode
        expect_error(createEstuaryCSA(x, axis, left, right, density, gs,
                                      mode = 1),
                     'inherits(mode, "character")', fixed = TRUE)
        expect_error(createEstuaryCSA(x, axis, left, right, density, gs,
                                      mode = c("default", "lines")),
                     'length(mode) == 1', fixed = TRUE)
        expect_error(createEstuaryCSA(x, axis, left, right, density, gs,
                                      mode = "test"),
                     'mode == "default" | mode == "lines"', fixed = TRUE)
        
        # no error
        expect_no_error(
            createEstuaryCSA(x, axis, left, right, density, gs, "default"))
        expect_no_error(
            createEstuaryCSA(x, axis, left, right, density, gs, "lines"))
        expect_no_error(
            createEstuaryCSA(x, axis, left, right, density, gs, "linesplit"))
    }
})

