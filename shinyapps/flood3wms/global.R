################################################################################
# global.R
################################################################################
# libraries
#####
# load the necessary packages
library(shiny)
library(shinyjs)
library(shiny.i18n)
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)
library(sp)
library(raster)
library(hyd1d)
library(hydflood)

#####
# add some additional functions
###
# convert string to lowercase with uppercase first letter
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/chartr.html
simpleCap <- function(x) {
    paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)))
}

###
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

###
# compute a dist for given map_bounds
withinDist <- function(x, y, bb) {
    # switch off warnings
    oldw <- getOption("warn")
    options(warn = -1)
    
    ns <- bb$north - bb$south
    ew <- bb$east - bb$west
    screen <- sqrt(ns*ns + ew*ew)
    res <- gWithinDistance(x, y, dist = screen / 3)
    
    # switch on warnings
    options(warn = oldw)
    
    # return distance
    return(res)
}

##########
# settings
###
# set english locale to produce english plot labels
# Sys.setlocale(category = "LC_MESSAGES", locale = "de_DE.utf8")
#https://stackoverflow.com/questions/47750273/shiny-application-get-browser-language-settings
#https://github.com/chrislad/multilingualShinyApp

###
# enable server based bookmarking
# enableBookmarking(store = "server")

##########
# set variables and initiate data needed for the computation
###
# define standard projections WGS 1984, ETRS 1989 UTM 33N, ETRS 1989 UTM 32N
crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
df.crs_comp <- data.frame(river = c("Elbe", "Rhine"),
                          crs = c("ETRS 1989 UTM 33N", "ETRS 1989 UTM 32N"))

###
# rivers
rivers <- c("Please select!", "Elbe", "Rhine")
df.from_to <- data.frame(river    = rivers, 
                         from     = c(NA, 0, 336.2),
                         to       = c(NA, 585.7, 865.7),
                         from_val = c(NA, 0, 336.2),
                         to_val   = c(NA, 585.7, 865.7))

###
# SpatialPointsDataFrame with all gauging stations
# data("df.gauging_station_data", package = "hyd1d")
# save(df.gauging_station_data, file = "df.gauging_station_data.rda")
# load("data/df.gauging_station_data.rda")
# df.gsd <- df.gauging_station_data[df.gauging_station_data$data_present,]
# df.gsd <- df.gsd[-which(df.gsd$river == "RHINE" & df.gsd$km_qps < 336.2), ]
# df.gsd$gauging_station <- asc2utf8(df.gsd$gauging_station)
# spdf.gsd <- SpatialPointsDataFrame(coords = df.gsd[,c("longitude", "latitude")], 
#                                   data = df.gsd, proj4string = crs)
# save(spdf.gsd, file = "data/spdf.gauging_station_data.rda")
# load("shinyapps/flood3wms/data/spdf.gauging_station_data.rda")
# spdf.gsd$gauging_station <- gsub("Ä", "AE", spdf.gsd$gauging_station)
# spdf.gsd$gauging_station <- gsub("Ö", "OE", spdf.gsd$gauging_station)
# spdf.gsd$gauging_station <- gsub("Ü", "UE", spdf.gsd$gauging_station)
# save(spdf.gsd, file = "shinyapps/flood3wms/data/spdf.gauging_station_data.rda")
load("data/spdf.gauging_station_data.rda")

###
# SpatialPolygonsDataFrames and data.frames of active floodplain polygons
# data(list = c("spdf.active_floodplain_rhine", "spdf.active_floodplain_elbe"), 
#      package = "hydflood")
# load("data/spdf.active_floodplain_elbe.rda")
# load("data/spdf.active_floodplain_rhine.rda")
# spdf.afe <- spTransform(spdf.active_floodplain_elbe, CRSobj = crs)
# spdf.afr <- spTransform(spdf.active_floodplain_rhine, CRSobj = crs)
# save(list = c("spdf.afe", "spdf.afr"), file = "data/spdf.afX.rda")
load("data/spdf.afX.rda")

# df.coor.afe <- as.data.frame(spdf.afe@polygons[[1]]@Polygons[[1]]@coords)
# df.coor.afr <- as.data.frame(spdf.afr@polygons[[1]]@Polygons[[1]]@coords)
# names(df.coor.afe) <- c("lon", "lat")
# names(df.coor.afr) <- c("lon", "lat")
# save(list = c("df.coor.afe", "df.coor.afr"), file = "data/df.coor.afX.rda")
load("data/df.coor.afX.rda")

###
# SpatialPointsDataFrame of stationing
# spdf.hectometer_elbe <- readOGR(dsn = "~/hydflood/data-raw", layer = "hectometer_elbe")
# spdf.he <- spTransform(spdf.hectometer_elbe, CRSobj = crs)
# spdf.he <- spdf.he[which(spdf.he$M100 <= 585.7),]
# spdf.hectometer_rhine <- readOGR(dsn = "~/hydflood/data-raw", layer = "hectometer_rhine")
# spdf.hr <- spTransform(spdf.hectometer_rhine, CRSobj = crs)
# spdf.he@data$river <- as.factor(rep("Elbe", nrow(spdf.he@data)))
# spdf.hr@data$river <- as.factor(rep("rhine", nrow(spdf.hr@data)))
# spdf.station <- rbind(spdf.he, spdf.hr)
# spdf.station@data <- cbind(spdf.station@data, as.data.frame(coordinates(spdf.station)[,1:2]))
# names(spdf.station@data) <- c("OBJECTID", "station", "river", "longitude", "latitude")
# spdf.station <- spdf.station[order(spdf.station$river, spdf.station$station),]
# save(spdf.station, file = "data/spdf.station.rda")
load("data/spdf.station.rda")

years <- seq(1990, as.numeric(strftime(Sys.Date(), "%Y")) - 1, 1)

url_base <- "https://geoportal.bafg.de/arcgis/services/flood3/2022_"

# https://geoportal.bafg.de/arcgis3/services/Flut3/2022_rhine_1990_1999/MapServer/WMSServer?

## options(shiny.trace = TRUE)

# translation
translator <- Translator$new(translation_json_path = "translation.json")

# JavaScript to determine browser language
jscode <- paste0("var language =  window.navigator.userLanguage || window.navi",
                 "gator.language;Shiny.onInputChange('lang', language);console",
                 ".log(language);")
de <- function(x) {
    if (is.null(x)) {return(FALSE)}
    if (startsWith(x, "de")) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
