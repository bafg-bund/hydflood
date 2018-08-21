# standard library path for the package install
R_version <- paste(sep = ".", R.Version()$major, R.Version()$minor)
lib <- paste0("~/R/", R_version, "/")

# load the necessary packages
library(shiny, lib.loc = lib)
library(leaflet, lib.loc = lib)
library(leaflet.extras, lib.loc = lib)
library(sp, lib.loc = lib)
library(raster, lib.loc = lib)
library(rgdal, lib.loc = lib)
library(rgeos, lib.loc = lib)
library(hyd1d, lib.loc = lib)
library(hydflood3, lib.loc = lib)
library(mapedit, lib.loc = lib)
library(mapview, lib.loc = lib)

# set english locale to produce english plot labels
Sys.setlocale(category = "LC_MESSAGES", locale = "en_US.utf8")
#https://stackoverflow.com/questions/47750273/shiny-application-get-browser-language-settings
#https://github.com/chrislad/multilingualShinyApp

# rivers
rivers <- c("Bitte wählen Sie!", "Elbe", "Rhein")
df.from_to <- data.frame(river    = rivers, 
                         from     = c(0, 0, 336.2),
                         to       = c(865.7, 585.7, 865.7),
                         from_val = c(0, 0, 336.2),
                         to_val   = c(865.7, 585.7, 865.7))

# https://stat.ethz.ch/R-manual/R-devel/library/base/html/chartr.html
simpleCap <- function(x) {
    paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)))
}

#####
# get spatial data from hyd1d and hydflood3
##
# define standard projection WGS 1984
crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

##
# gauging stations
data("df.gauging_station_data", package = "hyd1d", lib.loc = lib)
df.gsd <- df.gauging_station_data[df.gauging_station_data$data_present,]
spdf.gs <- SpatialPointsDataFrame(coords = df.gsd[,c("longitude", "latitude")], 
                                  data = df.gsd, proj4string = crs)

##
# gauging_data
data("df.gauging_data", package = "hyd1d", lib.loc = lib)
df.gd <- df.gauging_data

##
# active floodplains
data(list = c("spdf.active_floodplain_rhein", "spdf.active_floodplain_elbe"), 
     package = "hydflood3", lib.loc = lib)
spdf.afe <- spTransform(spdf.active_floodplain_elbe, CRSobj = crs)
spdf.afr <- spTransform(spdf.active_floodplain_rhein, CRSobj = crs)
df.coor.afe <- as.data.frame(spdf.afe@polygons[[1]]@Polygons[[1]]@coords)
df.coor.afr <- as.data.frame(spdf.afr@polygons[[1]]@Polygons[[1]]@coords)

names(df.coor.afe) <- c("lon", "lat")
names(df.coor.afr) <- c("lon", "lat")

##
# hectometer
#################
# rename to spdf.station
#################
# spdf.hectometer_elbe <- readOGR(dsn = "~/hydflood3/data-raw", layer = "hectometer_elbe")
# spdf.he <- spTransform(spdf.hectometer_elbe, CRSobj = crs)
# spdf.hectometer_rhein <- readOGR(dsn = "~/hydflood3/data-raw", layer = "hectometer_rhein")
# spdf.hr <- spTransform(spdf.hectometer_rhein, CRSobj = crs)
# spdf.he@data$river <- as.factor(rep("Elbe", nrow(spdf.he@data)))
# spdf.hr@data$river <- as.factor(rep("Rhein", nrow(spdf.hr@data)))
# spdf.station <- rbind(spdf.he, spdf.hr)
# spdf.station@data <- cbind(spdf.station@data, as.data.frame(coordinates(spdf.station)[,1:2]))
# names(spdf.station@data) <- c("OBJECTID", "station", "river", "longitude", "latitude")
# spdf.station <- spdf.station[order(spdf.station$river, spdf.station$station),]
# save(spdf.station, file = "~/hydflood3/data-raw/hectometer.rda")

load("~/hydflood3/data-raw/hectometer.rda")

# load("~/hydflood3/data-raw/spdf.active_floodplain_elbe_csa.rda")
# load("~/hydflood3/data-raw/spdf.active_floodplain_rhein_csa.rda")
# spdf.csae <-spTransform(spdf.active_floodplain_elbe_csa, CRSobj = crs)
# spdf.csar <-spTransform(spdf.active_floodplain_rhein_csa, CRSobj = crs)
# spdf2coor <- function(x){
#     y <- as.data.frame(x@Polygons[[1]]@coords)
#     names(y) <- c("lon", "lat")
#     return(y)
# }
# l.coor.csa.e <- lapply(spdf.csae@polygons, FUN = function(x){x@Polygons[[1]]@coords})
# l.coor.csa.r <- lapply(spdf.csar@polygons, FUN = function(x){x@Polygons[[1]]@coords})

##
# sf.area
sf.area <- NULL
spdf.area_verified <- NULL

##
# seq
seq_verified <- NULL

##
# email
email_verified <- NULL

##
# submitted
submitted <- NULL

