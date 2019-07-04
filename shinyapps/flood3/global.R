################################################################################
# global.R
################################################################################
# libraries
#####
# load the necessary packages
library(shiny)
library(shinyjs)
# library(shinycssloaders)
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(hyd1d)
library(hydflood3)

#####
# add some additional functions
###
# convert string to lowercase with uppercase first letter
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/chartr.html
simpleCap <- function(x) {
    paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)))
}

###
# convert byte substituted ascii strings to utf-8
# https://en.wikipedia.org/wiki/List_of_Unicode_characters
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

###
# validate an email adress
# after http://www.nicebread.de/validating-email-adresses-in-r/
isValidEmail <- function(x) {
    grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", 
          as.character(x), ignore.case = TRUE)
}

###
# generate a random string
# modified after https://ryouready.wordpress.com/2008/12/18/generate-random-string-name/
randomString <- function(length = 8) {
    paste0(sample(x = as.character(c(0:9, letters)), #, LETTERS
                  size = length, replace = TRUE), collapse = "")
}

###
# check, if the selected area intersects with the active floodplain
in_af <- function(area, af) {
    l.over <- sp::over(area, af, returnList = TRUE)
    if (length(unlist(l.over)) == 0) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

##########
# settings
###
# set english locale to produce english plot labels
Sys.setlocale(category = "LC_MESSAGES", locale = "de_DE.utf8")
#https://stackoverflow.com/questions/47750273/shiny-application-get-browser-language-settings
#https://github.com/chrislad/multilingualShinyApp

###
# enable server based bookmarking
enableBookmarking(store = "server")

##########
# set variables and initiate data needed for the computation
###
# define standard projections WGS 1984, ETRS 1989 UTM 33N, ETRS 1989 UTM 32N
crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
df.crs_comp <- data.frame(river = c("Elbe", "Rhein"),
                          crs = c("ETRS 1989 UTM 33N", "ETRS 1989 UTM 32N"))

###
# rivers
rivers <- c("Bitte wählen Sie!", "Elbe", "Rhein")
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
# df.gsd <- df.gsd[-which(df.gsd$river == "RHEIN" & df.gsd$km_qps < 336.2), ]
# df.gsd$gauging_station <- asc2utf8(df.gsd$gauging_station)
# spdf.gsd <- SpatialPointsDataFrame(coords = df.gsd[,c("longitude", "latitude")], 
#                                   data = df.gsd, proj4string = crs)
# save(spdf.gsd, file = "data/spdf.gauging_station_data.rda")
load("data/spdf.gauging_station_data.rda")

###
# df.gauging_data
# download daily refreshed dataset from aqualogy.de and rename it to df.gd
file_gd <- "data/df.gauging_data_latest.RDS"
url <- paste0("https://www.aqualogy.de/wp-content/uploads",
              "/bfg/df.gauging_data_latest.RDS")
if (! file.exists(file_gd)) {
    utils::download.file(url, file_gd, quiet = TRUE)
} else {
    if (file.info(file_gd)$ctime < 
        strptime(paste0(as.character(Sys.Date()), " 07:00:00"), 
                 format = "%Y-%m-%d %H:%M:%S")) {
        utils::download.file(url, file_gd, quiet = TRUE)
    }
}
df.gd <- readRDS(file_gd)

###
# SpatialPolygonsDataFrames and data.frames of active floodplain polygons
# data(list = c("spdf.active_floodplain_rhein", "spdf.active_floodplain_elbe"), 
#      package = "hydflood3")
# load("data/spdf.active_floodplain_elbe.rda")
# load("data/spdf.active_floodplain_rhein.rda")
# spdf.afe <- spTransform(spdf.active_floodplain_elbe, CRSobj = crs)
# spdf.afr <- spTransform(spdf.active_floodplain_rhein, CRSobj = crs)
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
# save(spdf.station, file = "data/spdf.station.rda")
load("data/spdf.station.rda")

