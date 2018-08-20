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

# set english locale to produce english plot labels
Sys.setlocale(category = "LC_MESSAGES", locale = "en_US.utf8")
#https://stackoverflow.com/questions/47750273/shiny-application-get-browser-language-settings
#https://github.com/chrislad/multilingualShinyApp

# rivers
rivers <- c("Elbe", "Rhein")
df.from_to <- data.frame(river    = rivers, 
                         from     = c(0, 336.2),
                         to       = c(585.7, 865.7),
                         from_val = c(257, 336.2),
                         to_val   = c(262, 362.4))

# https://stat.ethz.ch/R-manual/R-devel/library/base/html/chartr.html
simpleCap <- function(x) {
    paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)))
}

# https://groups.google.com/forum/#!topic/shiny-discuss/gj-PDGH0KuM
alignRight <- function(el) {
    htmltools::tagAppendAttributes(el,
                                   style="margin-left:auto;margin-right:none;"
    )
}

# get spatial data from hyd1d and hydflood3
data("df.gauging_station_data", package = "hyd1d", lib.loc = lib)
data(list = c("spdf.active_floodplain_rhein", "spdf.active_floodplain_elbe"), 
     package = "hydflood3", lib.loc = lib)

crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
spdf.afe <- spTransform(spdf.active_floodplain_elbe, CRSobj = crs)
spdf.afr <- spTransform(spdf.active_floodplain_rhein, CRSobj = crs)
coor.e <- as.data.frame(spdf.afe@polygons[[1]]@Polygons[[1]]@coords)
coor.r <- as.data.frame(spdf.afr@polygons[[1]]@Polygons[[1]]@coords)
names(coor.e) <- c("lon", "lat")
names(coor.r) <- c("lon", "lat")

load("/home/arnd/hydflood3/data-raw/spdf.active_floodplain_elbe_csa.rda")
load("/home/arnd/hydflood3/data-raw/spdf.active_floodplain_rhein_csa.rda")
spdf.csae <-spTransform(spdf.active_floodplain_elbe_csa, CRSobj = crs)
spdf.csar <-spTransform(spdf.active_floodplain_rhein_csa, CRSobj = crs)
spdf2coor <- function(x){
    y <- as.data.frame(x@Polygons[[1]]@coords)
    names(y) <- c("lon", "lat")
    return(y)
}
l.coor.csa.e <- lapply(spdf.csae@polygons, FUN = function(x){x@Polygons[[1]]@coords})
l.coor.csa.r <- lapply(spdf.csar@polygons, FUN = function(x){x@Polygons[[1]]@coords})
