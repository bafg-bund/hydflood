# function to convert a rasters extent to a polygon
rasterextent2polygon <- function(x) {
    e <- raster::extent(x)
    df.corners <- data.frame(x = c(e@xmin, e@xmax, e@xmax, e@xmin, e@xmin),
                             y = c(e@ymin, e@ymin, e@ymax, e@ymax, e@ymin))
    ma.corners <- as.matrix(df.corners)
    p.polygon <- sp::Polygon(ma.corners, FALSE)
    p.polygons <- sp::Polygons(list(p.polygon), ID = "1")
    sp.polygon <- sp::SpatialPolygons(list(p.polygons), 
                                      proj4string = raster::crs(x))
    return(sp.polygon)
}

# function to convert an extent to a polygon
extent2polygon <- function(x, crs) {
    df.corners <- data.frame(x = c(x@xmin, x@xmax, x@xmax, x@xmin, x@xmin),
                             y = c(x@ymin, x@ymin, x@ymax, x@ymax, x@ymin))
    ma.corners <- as.matrix(df.corners)
    p.polygon <- sp::Polygon(ma.corners, FALSE)
    p.polygons <- sp::Polygons(list(p.polygon), ID = "1")
    sp.polygon <- sp::SpatialPolygons(list(p.polygons), 
                                      proj4string = crs)
    return(sp.polygon)
}

utm32n <- sp::CRS(SRS_string = 
                      'PROJCRS["ETRS89 / UTM zone 32N",
    BASEGEOGCRS["ETRS89",
        DATUM["European Terrestrial Reference System 1989",
            ELLIPSOID["GRS 1980",6378137,298.257222101,
                LENGTHUNIT["metre",1]]],
        PRIMEM["Greenwich",0,
            ANGLEUNIT["degree",0.0174532925199433]],
        ID["EPSG",4258]],
    CONVERSION["UTM zone 32N",
        METHOD["Transverse Mercator",
            ID["EPSG",9807]],
        PARAMETER["Latitude of natural origin",0,
            ANGLEUNIT["degree",0.0174532925199433],
            ID["EPSG",8801]],
        PARAMETER["Longitude of natural origin",9,
            ANGLEUNIT["degree",0.0174532925199433],
            ID["EPSG",8802]],
        PARAMETER["Scale factor at natural origin",0.9996,
            SCALEUNIT["unity",1],
            ID["EPSG",8805]],
        PARAMETER["False easting",500000,
            LENGTHUNIT["metre",1],
            ID["EPSG",8806]],
        PARAMETER["False northing",0,
            LENGTHUNIT["metre",1],
            ID["EPSG",8807]]],
    CS[Cartesian,2],
        AXIS["(E)",east,
            ORDER[1],
            LENGTHUNIT["metre",1]],
        AXIS["(N)",north,
            ORDER[2],
            LENGTHUNIT["metre",1]],
    USAGE[
        SCOPE["Engineering survey, topographic mapping."],
        AREA["Europe between 6 E and 12 E: Austria; Belgium; Denmark - onshore a
            nd offshore; Germany - onshore and offshore; Norway including - ons
            hore and offshore; Spain - offshore."],
        BBOX[38.76,6,83.92,12]],
    ID["EPSG",25832]]')

isUTM32 <- function(x) {
    if (class(x)[[1]] != "CRS") {
        stop("'x' must be class 'CRS'.")
    }
    
    args <- x@projargs
    t <- c("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
           "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs",
           "+init=epsg:25832 +proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
           "+init=epsg:25832 +proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")
    if (args %in% t) {
        return(TRUE)
    }
    return(FALSE)
}

utm33n <- sp::CRS(SRS_string = 
                      'PROJCRS["ETRS89 / UTM zone 33N",
    BASEGEOGCRS["ETRS89",
        DATUM["European Terrestrial Reference System 1989",
            ELLIPSOID["GRS 1980",6378137,298.257222101,
                LENGTHUNIT["metre",1]]],
        PRIMEM["Greenwich",0,
            ANGLEUNIT["degree",0.0174532925199433]],
        ID["EPSG",4258]],
    CONVERSION["UTM zone 33N",
        METHOD["Transverse Mercator",
            ID["EPSG",9807]],
        PARAMETER["Latitude of natural origin",0,
            ANGLEUNIT["degree",0.0174532925199433],
            ID["EPSG",8801]],
        PARAMETER["Longitude of natural origin",15,
            ANGLEUNIT["degree",0.0174532925199433],
            ID["EPSG",8802]],
        PARAMETER["Scale factor at natural origin",0.9996,
            SCALEUNIT["unity",1],
            ID["EPSG",8805]],
        PARAMETER["False easting",500000,
            LENGTHUNIT["metre",1],
            ID["EPSG",8806]],
        PARAMETER["False northing",0,
            LENGTHUNIT["metre",1],
            ID["EPSG",8807]]],
    CS[Cartesian,2],
        AXIS["(E)",east,
            ORDER[1],
            LENGTHUNIT["metre",1]],
        AXIS["(N)",north,
            ORDER[2],
            LENGTHUNIT["metre",1]],
    USAGE[
        SCOPE["Engineering survey, topographic mapping."],
        AREA["Europe between 12 E and 18 E: Austria; Denmark - offshore and offs
            hore; Germany - onshore and offshore; Norway including Svalbard - on
            shore and offshore."],
        BBOX[46.4,12,84.01,18.01]],
    ID["EPSG",25833]]')

isUTM33 <- function(x) {
    if (class(x)[[1]] != "CRS") {
        stop("'x' must be class 'CRS'.")
    }
    
    args <- x@projargs
    t <- c("+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
           "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs",
           "+init=epsg:25833 +proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
           "+init=epsg:25833 +proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")
    if (args %in% t) {
        return(TRUE)
    }
    return(FALSE)
}


