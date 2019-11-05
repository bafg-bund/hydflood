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

# convert byte substituted ascii strings to utf-8
# https://en.wikipedia.org/wiki/List_of_Unicode_characters
# 
asc2utf8 <- function(x){
    y <- iconv(x, "ASCII", "UTF-8", sub="byte")
    # Ä
    y <- sub("<c3><84>", "\u00c4", y)
    # ä
    y <- sub("<c3><a4>", "\u00e4", y)
    # Ö
    y <- sub("<c3><96>", "\u00d6", y)
    # ö
    y <- sub("<c3><b6>", "\u00f6", y)
    # Ü
    y <- sub("<c3><9c>", "\u00dc", y)
    # ü
    y <- sub("<c3><bc>", "\u00fc", y)
    return(y)
}
