################################################################################
# datx_estuary_elbe.R
# 
# author: arnd.weber@bafg.de
# date:   25.08.2023
# 
# purpose:
#    - prepare input and test data for R/createEstuaryCSA.R
#    - prepare tiles, CSA and DEM data
# 
################################################################################
library(hydflood)

# set path
# path <- "~/freigaben/Projekte/Elbe_U/EL_586_752_Morphozonen/scripts/2022/"

# import the required geometries and other data
# area
# x <- st_read(paste0(path, "data/ug_Deichlinien2016.shp"))
# x[, names(x)[names(x) != "geometry"]] <- NULL
# x <- st_union(x)
# st_write(x, dsn = "data-raw", layer = "estuary_area",
#          driver = "ESRI Shapefile", delete_layer = TRUE)
x <- st_read("~/hydflood/data-raw", "estuary_area", quiet = TRUE)

# axis
# axis <-  st_zm(st_geometry(st_read(paste0(path,
#                                           "data/UG_GAchse_2022.04.01.shp"))),
#                na.rm = TRUE)
# st_write(axis, "data-raw", "estuary_axis", driver = "ESRI Shapefile",
#          delete_layer = TRUE)
axis <- st_read("~/hydflood/data-raw", "estuary_axis", quiet = TRUE)

# left
# left <- st_read(paste0(path, "data/UG_Linie_unten_2022.04.01.shp"))
# left[, names(left)[names(left) != "geometry"]] <- NULL
# st_write(left, "data-raw", "estuary_left", driver = "ESRI Shapefile",
#          delete_layer = TRUE)
left <- st_read("~/hydflood/data-raw", "estuary_left", quiet = TRUE)

# right
# right <- st_read(paste0(path, "data/UG_Linie_oben_2022.04.01.shp"))
# right[, names(right)[names(right) != "geometry"]] <- NULL
# st_write(right, "data-raw", "estuary_right", driver = "ESRI Shapefile",
#          delete_layer = TRUE)
right <- st_read("~/hydflood/data-raw", "estuary_right", quiet = TRUE)

# gs
# gs <- st_read(paste0(path, "data/Pegel_GK_Wiski_neu.shp"))
# gs <- gs[,c("PEGELDATEN", "geometry")]
# names(gs) <- c("ga_station", "geometry")
# st_write(gs, "data-raw", "estuary_gs", driver = "ESRI Shapefile",
#          delete_layer = TRUE)
gs <- st_read("~/hydflood/data-raw", "estuary_gs", quiet = TRUE)

#####
# create CSA raster files
if (!exists("data-raw/estuary_csa.shp")) {
    sf.estuary_csa <- createEstuaryCSA(x, axis, left, right, 1/100, gs, "lines")
    names(sf.estuary_csa)[3] <- "station_in"
    st_write(sf.estuary_csa, "data-raw", "estuary_csa",
             driver = "ESRI Shapefile")
} else {
    sf.estuary_csa <- st_read("data-raw", "estuary_csa")
}

tiles <- createTiles(x, 10000, 10000)
if (!file.exists("data-raw/estuary_tiles.shp")) {
    st_write(tiles, "data-raw", "estuary_tiles", driver = "ESRI Shapefile")
}
sv.estuary_csa <- vect(sf.estuary_csa)

for (i in 1:nrow(tiles)) {
    a_tile <- vect(tiles[i, ])
    filename <- paste0(options()$hydflood.datadir, "/ee_", a_tile$tile_ID,
                       "_CSA.tif")
    if (!file.exists(filename)) {
        r <- rast(a_tile, nrows = 10000, ncols = 10000, resolution = 1)
        r_csa <- rasterize(sv.estuary_csa, r, field = "station_in",
                           filename = filename,
                           wopt = list(gdal = c("COMPRESS=LZW", "TFW=NO"),
                                       datatype = "INT4U"), overwrite = TRUE)
    }
}

#####
# DEM

