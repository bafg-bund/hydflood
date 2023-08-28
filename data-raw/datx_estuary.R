################################################################################
# datx_estuary.R
# 
# author: arnd.weber@bafg.de
# date:   25.08.2023
# 
# purpose:
#    - prepare input and test data for R/createEstuaryCSA.R
# 
################################################################################

# set path
path <- "~/freigaben/Projekte/Elbe_U/EL_586_752_Morphozonen/scripts/2022/"

# import the required geometries and other data
# area
x <- st_read(paste0(path, "data/ug_Deichlinien2016.shp"))
x[, names(x)[names(x) != "geometry"]] <- NULL
x <- st_union(x)
st_write(x, dsn = "data-raw", layer = "estuary_area",
         driver = "ESRI Shapefile", delete_layer = TRUE)

# axis
axis <-  st_zm(st_geometry(st_read(paste0(path,
                                          "data/UG_GAchse_2022.04.01.shp"))),
               na.rm = TRUE)
st_write(axis, "data-raw", "estuary_axis", driver = "ESRI Shapefile",
         delete_layer = TRUE)

# left
left <- st_read(paste0(path, "data/UG_Linie_unten_2022.04.01.shp"))
left[, names(left)[names(left) != "geometry"]] <- NULL
st_write(left, "data-raw", "estuary_left", driver = "ESRI Shapefile",
         delete_layer = TRUE)

# right
right <- st_read(paste0(path, "data/UG_Linie_oben_2022.04.01.shp"))
right[, names(right)[names(right) != "geometry"]] <- NULL
st_write(right, "data-raw", "estuary_right", driver = "ESRI Shapefile",
         delete_layer = TRUE)

# gs
gs <- st_read(paste0(path, "data/Pegel_GK_Wiski_neu.shp"))
gs <- gs[,c("PEGELDATEN", "geometry")]
names(gs) <- c("ga_station", "geometry")
st_write(gs, "data-raw", "estuary_gs", driver = "ESRI Shapefile",
         delete_layer = TRUE)

