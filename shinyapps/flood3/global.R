################################################################################
# global.R
################################################################################
#setwd("shinyapps/flood3")

# libraries
#####
# load the necessary packages
library(shiny)
library(shinyjs)
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(sp)
library(raster)
library(hyd1d)
library(hydflood)
library(tidyverse)

#####
# add some additional functions
###
# convert string to lowercase with uppercase first letter
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/chartr.html
simpleCap <- function(x) {
    paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)))
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
    l.over <- af[area,]
    if (nrow(l.over) == 0) {
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
crs <- st_crs(4326)
df.crs_comp <- data.frame(river = c("Elbe", "Rhine"),
                          crs = c("ETRS 1989 UTM 33N", "ETRS 1989 UTM 32N"))

###
# rivers
rivers <- c("Bitte wählen Sie!", "Elbe", "Rhine")
df.from_to <- data.frame(river    = rivers, 
                         from     = c(NA, 0, 336.2),
                         to       = c(NA, 585.7, 865.7),
                         from_val = c(NA, 0, 336.2),
                         to_val   = c(NA, 585.7, 865.7))

###
# sf with all gauging stations
# data("df.gauging_station_data", package = "hyd1d")
# df.gsd <- df.gauging_station_data[df.gauging_station_data$data_present,]
# df.gsd <- df.gsd[-which(df.gsd$river == "RHINE" & df.gsd$km_qps < 336.2), ]
# df.gsd <- df.gsd[-which(df.gsd$river == "WESER"), ]
# sf.gsd <- st_as_sf(df.gsd, coords = c("longitude", "latitude"), crs = crs,
#                    remove = FALSE)
# save(sf.gsd, file = "data/sf.gsd.rda")
load("data/sf.gsd.rda")

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
# sf and data.frames of active floodplain polygons
# data(list = c("sf.afr", "sf.afe"), package = "hydflood")
# sf.afe <- st_transform(sf.af(name = "Elbe"), crs)
# sf.afr <- st_transform(sf.af(name = "Rhine"), crs)
# save(list = c("sf.afe", "sf.afr"), file = "data/sf.afX.rda")
load("data/sf.afX.rda")

# df.coor.afe <- as.data.frame(st_coordinates(sf.afe))
# df.coor.afe <- df.coor.afe[which(df.coor.afe$L1 == 1),]
# df.coor.afe <- df.coor.afe[, c("X", "Y")]
# names(df.coor.afe) <- c("lon", "lat")
# df.coor.afr <- as.data.frame(st_coordinates(sf.afr))
# df.coor.afr <- df.coor.afr[which(df.coor.afr$L1 == 1),]
# df.coor.afr <- df.coor.afr[, c("X", "Y")]
# names(df.coor.afr) <- c("lon", "lat")
# save(list = c("df.coor.afe", "df.coor.afr"), file = "data/df.coor.afX.rda")
load("data/df.coor.afX.rda")

###
# sf of stationing
# sf.hectometer_elbe <- st_read(dsn = "~/hydflood/data-raw", layer = "hectometer_elbe")
# sf.he <- st_transform(sf.hectometer_elbe, crs)
# sf.hectometer_rhine <- st_read(dsn = "~/hydflood/data-raw", layer = "hectometer_rhine")
# sf.hr <- st_transform(sf.hectometer_rhine, crs)
# sf.he$river <- as.factor(rep("Elbe", nrow(sf.he)))
# sf.hr$river <- as.factor(rep("Rhine", nrow(sf.hr)))
# sf.station <- rbind(sf.he, sf.hr)
# names(sf.station)[1:3] <- c("OBJECTID", "station", "river")
# sf.station$longitude <- as.data.frame(st_coordinates(sf.station)[,1:2])[, 1]
# sf.station$latitude <- as.data.frame(st_coordinates(sf.station)[,1:2])[, 2]
# sf.station <- sf.station[order(sf.station$river, sf.station$station),]
# save(sf.station, file = "data/sf.station.rda")
load("data/sf.station.rda")

