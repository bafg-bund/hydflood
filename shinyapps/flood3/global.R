# standard library path for the package install
R_version <- paste(sep = ".", R.Version()$major, R.Version()$minor)
lib <- paste0("~/R/", R_version, "/")

# load the necessary packages
library(shiny, lib.loc = lib)
library(shinyjs, lib.loc = lib)
library(leaflet, lib.loc = lib)
library(leaflet.extras, lib.loc = lib)
library(sp, lib.loc = lib)
library(raster, lib.loc = lib)
library(rgdal, lib.loc = lib)
library(rgeos, lib.loc = lib)
library(hyd1d, lib.loc = lib)
library(hydflood3, lib.loc = lib)
library(htmltools, lib.loc = lib)
# library(mailR, lib.loc = lib)
# library(mapedit, lib.loc = lib)
# library(mapview, lib.loc = lib)

# set english locale to produce english plot labels
Sys.setlocale(category = "LC_MESSAGES", locale = "en_US.utf8")
#https://stackoverflow.com/questions/47750273/shiny-application-get-browser-language-settings
#https://github.com/chrislad/multilingualShinyApp

# rivers
rivers <- c("Bitte wählen Sie!", "Elbe", "Rhein")
df.from_to <- data.frame(river    = rivers, 
                         from     = c(NA, 0, 336.2),
                         to       = c(NA, 585.7, 865.7),
                         from_val = c(NA, 0, 336.2),
                         to_val   = c(NA, 585.7, 865.7))

# https://stat.ethz.ch/R-manual/R-devel/library/base/html/chartr.html
simpleCap <- function(x) {
    paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)))
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

# after http://www.nicebread.de/validating-email-adresses-in-r/
isValidEmail <- function(x) {
    grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", 
          as.character(x), ignore.case = TRUE)
}

# modified after https://ryouready.wordpress.com/2008/12/18/generate-random-string-name/
randomString <- function(length = 8) {
    paste0(sample(x = as.character(c(0:9, letters, LETTERS)), 
                  size = length, replace = TRUE), collapse = "")
}

# function to check weather the selected area intersects with the active 
# floodplain
in_af <- function(area, af) {
    l.over <- sp::over(area, af, returnList = TRUE)
    if (length(unlist(l.over)) == 0) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

#####
# get spatial data from hyd1d and hydflood3
##
# define standard projection WGS 1984
crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

df.crs_comp <- data.frame(river = c("Elbe", "Rhein"),
                          crs = c("ETRS 1989 UTM 33N", "ETRS 1989 UTM 32N"))

##
# gauging stations
#data("df.gauging_station_data", package = "hyd1d", lib.loc = lib)
load("data/df.gauging_station_data.rda")
df.gsd <- df.gauging_station_data[df.gauging_station_data$data_present,]
df.gsd$gauging_station <- asc2utf8(df.gsd$gauging_station)
spdf.gs <- SpatialPointsDataFrame(coords = df.gsd[,c("longitude", "latitude")], 
                                  data = df.gsd, proj4string = crs)

##
# gauging_data
#data("df.gauging_data", package = "hyd1d", lib.loc = lib)
file_gd <- "data/df.gauging_data_latest.rda"
url <- paste0("https://www.aqualogy.de/wp-content/uploads",
              "/bfg/df.gauging_data_latest.rda")
if (! file.exists(file_gd)) {
    utils::download.file(url, file_gd, quiet = TRUE)
} else {
    if (file.info(file_gd)$ctime < 
        strptime(paste0(as.character(Sys.Date()), " 07:00:00"), 
                 format = "%Y-%m-%d %H:%M:%S")) {
        utils::download.file(url, file_gd, quiet = TRUE)
    }
}
load(file_gd)
df.gd <- df.gauging_data

##
# active floodplains
# data(list = c("spdf.active_floodplain_rhein", "spdf.active_floodplain_elbe"), 
#      package = "hydflood3", lib.loc = lib)
load("data/spdf.active_floodplain_elbe.rda")
load("data/spdf.active_floodplain_rhein.rda")
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
load("data/hectometer.rda")

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


enableBookmarking(store = "server")

#readRDS(file = "/var/lib/shiny-server/bookmarks/WeberA/07-flood3-84cfb52af5f2bdce0cbe624d35e3e2b9/ef808af0787061e6/input.rds")
