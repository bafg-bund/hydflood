################################################################################
# graphical_abstract.R
#
# author: arnd.weber@bafg.de
# date:   20.07.2022
#
# purpose:
#   - 3D plots with water level data near Dessau
#
################################################################################
# set language
Sys.setlocale(category = "LC_MESSAGES", locale = "en_US.utf8")

# select time
date_min <- as.POSIXct("2013-01-01")
date_max <- as.POSIXct("2013-12-31")
j <- as.numeric(154)
h <- seq(date_min, date_max, "days")[j]
year <- strftime(date_min, "%Y")
year_p <- as.character(as.numeric(year) + 1)

# 3D settings
phi <- 17
theta <- 150
lphi <- 120
ltheta <- 30
zmin <- 48
zmax <- 65

# configure output
verbose <- FALSE
quiet <- !verbose

# output paths
out_dir <- paste0("vignettes/graphical_abstract")
dir.create(out_dir, verbose, TRUE)

#####
# load the packages
library(sp)
library(raster)
library(rgdal)
library(plot3D)
library(hyd1d)
library(hydflood)
library(plotrix)

#####
# additional functions
##
# Add an alpha value to a colour
add.alpha <- function(col, alpha=1){
    if(missing(col))
        stop("Please provide a vector of colours.")
    apply(sapply(col, col2rgb)/255, 2, 
          function(x) 
              rgb(x[1], x[2], x[3], alpha=alpha))  
}

# color function for the DEM and WL
dem_colfunc <- colorRampPalette(c("saddlebrown", "yellow", "darkgreen"))
wl_colfunc <- colorRampPalette(c("darkblue", "cornflowerblue"))

#####
# GIS data
##
# visualisation extent
ext <- extent(308050, 310000, 5748500, 5752000)
sfp.ext <- hydflood:::extent2polygon(ext, st_crs(25833))

# active floodplain
spdf.afe <- as(sf.af("Elbe"), "Spatial")
spdf.afe <- crop(spdf.afe, ext)
sfp.afe <- st_as_sf(spdf.afe)

# axis
sldf.axis <- readOGR("data-raw", "doc_axis_dessau")
sldf.axis <- crop(sldf.axis, ext)
sfl.axis <- st_as_sf(sldf.axis)

# mask
spdf.mask <- readOGR("data-raw", "doc_abstract_ext")
spdf.mask_s <- spdf.mask[which(spdf.mask$side == "south"),]
sfp.mask <- st_as_sf(spdf.mask)
sfp.mask_s <- st_as_sf(spdf.mask_s)

# gauging stations
df.gs <- df.gauging_station_data[df.gauging_station_data$river == "ELBE" &
                                     df.gauging_station_data$data_present,]
sf.gs <- st_as_sf(df.gs, coords = c("longitude", "latitude"), remove = FALSE,
                  crs = st_crs(4326))
spdf.gs <- as(st_transform(sf.gs, st_crs(25833)), "Spatial")
spdf.gs <- spdf.gs[spdf.mask,]

# import hectometer
spdf.hectometer <- readOGR("data-raw", "doc_hectometer_dessau", 
                           verbose = verbose, pointDropZ = TRUE)
id <- which(spdf.hectometer$M100 >= 257 & spdf.hectometer$M100 <= 262)
spdf.hectometer <- spdf.hectometer[id,]
spdf.hectometer <- crop(spdf.hectometer, ext)
sp.hectometer <- SpatialPoints(spdf.hectometer, 
                               proj4string = crs(spdf.afe))

##
# import and convert raster data
# dem
raster.dem <- raster("data-raw/raster.dem_dessau.tif")
raster.dem <- crop(raster.dem, ext)
raster.dem <- mask(raster.dem, spdf.mask_s)

# get rid of some dem artefacts
raster.dem[raster.dem > 62] <- 62

# aggregate to decrease resolution
raster.dem <- aggregate(raster.dem, 20)

df.dem <- as.data.frame(raster.dem, xy = TRUE, na.rm = FALSE)
ma.dem <- as.matrix(raster.dem)

# csa
raster.csa <- raster("data-raw/raster.csa_dessau.tif")
raster.csa <- crop(raster.csa, ext)
raster.csa <- mask(raster.csa, spdf.mask_s)
raster.csa <- aggregate(raster.csa, 20, median)
df.csa <- as.data.frame(raster.csa, xy = TRUE, na.rm = FALSE)
ma.csa <- as.matrix(raster.csa)

#####
# gauging data
# subset df.gauging_data for w plotting
df.gauging_data <- readRDS("~/.hyd1d/df.gauging_data_latest.RDS")
id <- which(df.gauging_data$gauging_station %in% spdf.gs$gauging_station &
                df.gauging_data$date >= as.Date(date_min) &
                df.gauging_data$date <= as.Date(date_max))
df.gd <- df.gauging_data[id,]
pnp <- spdf.gs$pnp
names(pnp) <- spdf.gs$gauging_station
mw <- pnp + spdf.gs$mw
df.gd$wl <- pnp[df.gd$gauging_station] + df.gd$w/100
df.gd_r <- df.gd[df.gd$gauging_station == "ROSSLAU",]
wl_r <- df.gd_r$wl[j]
df.gd_d <- df.gd[df.gd$gauging_station == "DESSAU",]
wl_d <- df.gd_d$wl[j]

#####
# wldf
station_int <- as.integer(unique(raster.csa))
wldf_template <- WaterLevelDataFrame(river = "Elbe", time = as.POSIXct(NA),
                                     station_int = station_int)

# compute a water level
wldf <- wldf_template
setTime(wldf) <- h
wldf <- waterLevel(wldf, TRUE)

# transfer water level information to ma.dem and raster.dem
ma.wl <- ma.dem
raster.wl <- raster(raster.dem)
for (s in station_int) {
    ma.wl[ma.csa == s] <- wldf$w[wldf$station_int == s]
    raster.wl[raster.csa == s] <- wldf$w[wldf$station_int == s]
}

ma.wl[ma.wl <= ma.dem] <- NA
raster.wl[raster.wl <= raster.dem] <- NA

# query wl at hectometers a
spdf.temp <- raster::extract(x = raster.wl, 
                             y = sp.hectometer, 
                             sp = TRUE)
spdf.temp_hec <- spdf.hectometer
spdf.temp_hec@data <- cbind(spdf.temp_hec@data, spdf.temp@data$layer)
names(spdf.temp_hec@data)[length(names(spdf.temp_hec@data))] <- "wl"

#####
# overview
pdf(file = paste0(out_dir, "/01_overview.pdf"), width = 13, height = 5)

# set layout 1:3:1
layout(matrix(c(1, 2, 4,
                1, 3, 4), 2, 3, byrow = TRUE), widths = c(1, 3, 1))

# plot w sequence
plot(x = df.gd_r$date, y = df.gd_r$wl, type = "l", ylim = c(54, 61), 
     xlab = NA, xaxt = "n", ylab = "water level (m a.s.l.)", 
     col = 1, lty = 1, main = "ROSSLAU", bty = "n")
points(as.Date(h), wl_r, cex = 1, pch = 21, col = 1, bg = NA)
axis.Date(1, at = as.Date(paste0(year, c("-02-01", "-03-01", "-04-01",
                                         "-05-01", "-06-01", "-08-01", 
                                         "-09-01", "-10-01", "-11-01",
                                         "-12-01"))), 
          tck = -0.02, labels = FALSE)
axis.Date(1, at = c(as.Date(paste0(year, "-01-01")), 
                    as.Date(paste0(year, "-07-01")), 
                    as.Date(paste0(year_p, "-01-01"))), format = "%Y-%m-%d", 
          tck = -0.05, labels = TRUE)

# plot hyd1d
plotShiny(wldf, TRUE, TRUE, TRUE,
          xlim = c(min(station_int)/1000, max(station_int)/1000),
          ylim = c(54, 61),
          xlab = "river kilometer", ylab = "water level (m a.s.l.)")

# plot3D
# dem
persp3D(x = unique(df.dem$y), y = - unique(df.dem$x), z = ma.dem, 
        col = dem_colfunc((62 - 50)*2), 
        # clim = c(50, 62),
        # colkey = list(length = 0.3,
        #               width = 0.4,
        #               shift = 0.2), 
        # clab = c("m a.s.l.", "", "DEM"),
        zlim = c(zmin, zmax), expand = 10, box = FALSE,
        scale = FALSE, plot = TRUE, phi = phi, theta = theta,
        lighting = TRUE, lphi = lphi, ltheta = ltheta, shade = 0.5)

# water level
persp3D(x = unique(df.dem$y), y = - unique(df.dem$x), z = ma.wl,
        add = TRUE,
        col = add.alpha(wl_colfunc(20), 0.5),
        clim = c(54, 60.6),
        colkey = list(length = 0.3,
                      width = 0.4,
                      shift = -0.2),
        clab = c("water level"))

# hectometer
id_kilo <- which(spdf.temp_hec@data$M100 %in% c(258, 259, 260, 261))
# c(258, 258.5, 259, 259.5, 260, 260.5, 261, 261.5)
scatter3D(x = coordinates(spdf.temp_hec)[id_kilo, 2], 
          y = - coordinates(spdf.temp_hec)[id_kilo, 1], 
          z = spdf.temp_hec@data$wl[id_kilo], 
          type = "p", pch = 21, col = "black", bg = "grey", cex = 0.7, 
          add = TRUE)
text3D(x = coordinates(spdf.temp_hec)[id_kilo, 2], 
       y = - coordinates(spdf.temp_hec)[id_kilo, 1], 
       z = spdf.temp_hec@data$wl[id_kilo] + 3, 
       label = spdf.temp_hec@data$M100[id_kilo], 
       col = "black", cex = 0.7, font = 2,
       add = TRUE)
scatter3D(x = coordinates(spdf.gs)[, 2],
          y = - coordinates(spdf.gs)[, 1],
          z = c(wl_r, wl_d),
          type = "p", pch = 21, col = "black", bg = c("white", "black"),
          cex = 1.5, add = TRUE)
text3D(x = coordinates(spdf.gs)[, 2],
       y = - coordinates(spdf.gs)[, 1],
       z = c(wl_r, wl_d) + 10,
       label = spdf.gs$gauging_station, 
       col = "black", cex = 1.5, font = 2, add = TRUE)

# plot w sequence
plot(x = df.gd_d$date, y = df.gd_d$wl, type = "l", ylim = c(54, 61), 
     xlab = NA, xaxt = "n", ylab = "water level (m a.s.l.)",
     col = 1, lty = 1, main = "DESSAU", bty = "n")
points(as.Date(h), wl_d, cex = 1, pch = 21, col = 1, bg = 1)
axis.Date(1, at = as.Date(paste0(year, c("-02-01", "-03-01", "-04-01",
                                         "-05-01", "-06-01", "-08-01", 
                                         "-09-01", "-10-01", "-11-01",
                                         "-12-01"))), 
          tck = -0.02, labels = FALSE)
axis.Date(1, at = c(as.Date(paste0(year, "-01-01")), 
                    as.Date(paste0(year, "-07-01")), 
                    as.Date(paste0(year_p, "-01-01"))), format = "%Y-%m-%d", 
          tck = -0.05, labels = TRUE)
axis(2, at = c(55, 57, 59, 61), tck = -0.02, labels = FALSE)

# close plotting device
dev.off()

#####
# timeseries Rosslau
pdf(file = paste0(out_dir, "/02_rosslau.pdf"), width = 5, height = 5)

# plot w sequence
plot(x = df.gd_r$date, y = df.gd_r$wl, type = "l", ylim = c(54, 61), 
     xlab = NA, xaxt = "n", ylab = "water level (m a.s.l.)", 
     col = 1, lty = 1, main = "ROSSLAU", bty = "n")
points(as.Date(h), wl_r, cex = 1, pch = 21, col = 1, bg = 1)
axis.Date(1, at = as.Date(paste0(year, c("-02-01", "-03-01", "-04-01",
                                         "-05-01", "-06-01", "-08-01", 
                                         "-09-01", "-10-01", "-11-01",
                                         "-12-01"))), 
          tck = -0.02, labels = FALSE)
axis.Date(1, at = c(as.Date(paste0(year, "-01-01")), 
                    as.Date(paste0(year, "-07-01")), 
                    as.Date(paste0(year_p, "-01-01"))), format = "%Y-%m-%d", 
          tck = -0.05, labels = TRUE)
axis(2, at = c(55, 57, 59, 61), tck = -0.02, labels = FALSE)
dev.off()

#####
# timeseries Dessau
pdf(file = paste0(out_dir, "/05_dessau.pdf"), width = 5, height = 5)

# plot w sequence
plot(x = df.gd_d$date, y = df.gd_d$wl, type = "l", ylim = c(54, 61), 
     xlab = NA, xaxt = "n", ylab = "water level (m a.s.l.)", 
     col = 1, lty = 1, main = "DESSAU", bty = "n")
points(as.Date(h), wl_d, cex = 1, pch = 21, col = 1, bg = NA)
axis.Date(1, at = as.Date(paste0(year, c("-02-01", "-03-01", "-04-01",
                                         "-05-01", "-06-01", "-08-01", 
                                         "-09-01", "-10-01", "-11-01",
                                         "-12-01"))), 
          tck = -0.02, labels = FALSE)
axis.Date(1, at = c(as.Date(paste0(year, "-01-01")), 
                    as.Date(paste0(year, "-07-01")), 
                    as.Date(paste0(year_p, "-01-01"))), format = "%Y-%m-%d", 
          tck = -0.05, labels = TRUE)
axis(2, at = c(55, 57, 59, 61), tck = -0.02, labels = FALSE)
dev.off()

#####
# waterlevel
pdf(file = paste0(out_dir, "/03_waterlevel.pdf"), width = 15, height = 5)
plotShiny(wldf, TRUE, TRUE, TRUE,
          xlim = c(min(station_int)/1000, max(station_int)/1000),
          xlab = "river kilometer", ylab = "water level (m a.s.l.)",
          ylim = c(54, 61))
dev.off()

#####
# 3D
##
# import and convert raster data
# dem
raster.dem <- raster("data-raw/raster.dem_dessau.tif")
raster.dem <- crop(raster.dem, ext)
raster.dem <- mask(raster.dem, spdf.mask_s)

# get rid of some dem artefacts
raster.dem[raster.dem > 62] <- 62

df.dem <- as.data.frame(raster.dem, xy = TRUE, na.rm = FALSE)
ma.dem <- as.matrix(raster.dem)

# csa
raster.csa <- raster("data-raw/raster.csa_dessau.tif")
raster.csa <- crop(raster.csa, ext)
raster.csa <- mask(raster.csa, spdf.mask_s)
df.csa <- as.data.frame(raster.csa, xy = TRUE, na.rm = FALSE)
ma.csa <- as.matrix(raster.csa)

ma.wl <- ma.dem
raster.wl <- raster(raster.dem)
for (s in station_int) {
    ma.wl[ma.csa == s] <- wldf$w[wldf$station_int == s]
    raster.wl[raster.csa == s] <- wldf$w[wldf$station_int == s]
}

ma.wl[ma.wl <= ma.dem] <- NA
raster.wl[raster.wl <= raster.dem] <- NA

# pdf(file = paste0(out_dir, "/04_3D.pdf"), width = 15, height = 5)
# # dem
# persp3D(x = unique(df.dem$y), y = - unique(df.dem$x), z = ma.dem, 
#         col = dem_colfunc((62 - 50)*2), 
#         # clim = c(50, 62),
#         # colkey = list(length = 0.3,
#         #               width = 0.4,
#         #               shift = 0.2), 
#         # clab = c("m a.s.l.", "", "DEM"),
#         zlim = c(zmin, zmax), expand = 10, box = FALSE,
#         scale = FALSE, plot = TRUE, phi = 10, theta = theta,
#         lighting = TRUE, lphi = lphi, ltheta = ltheta, shade = 0.5)
# 
# # water level
# persp3D(x = unique(df.dem$y), y = - unique(df.dem$x), z = ma.wl,
#         add = TRUE,
#         col = add.alpha(wl_colfunc(20), 0.5),
#         clim = c(54, 60.6),
#         colkey = list(length = 0.3,
#                       width = 0.4,
#                       shift = -0.2),
#         clab = c("water level"))
# 
# # hectometer
# id_kilo <- which(spdf.temp_hec@data$M100 %in% c(258, 259, 260, 261))
# # c(258, 258.5, 259, 259.5, 260, 260.5, 261, 261.5)
# scatter3D(x = coordinates(spdf.temp_hec)[id_kilo, 2], 
#           y = - coordinates(spdf.temp_hec)[id_kilo, 1], 
#           z = spdf.temp_hec@data$wl[id_kilo], 
#           type = "p", pch = 21, col = "black", bg = "grey", cex = 0.7, 
#           add = TRUE)
# text3D(x = coordinates(spdf.temp_hec)[id_kilo, 2], 
#        y = - coordinates(spdf.temp_hec)[id_kilo, 1], 
#        z = spdf.temp_hec@data$wl[id_kilo] + 3, 
#        label = spdf.temp_hec@data$M100[id_kilo], 
#        col = "white", cex = 1, font = 2, add = TRUE)
# text3D(x = coordinates(spdf.temp_hec)[id_kilo, 2], 
#        y = - coordinates(spdf.temp_hec)[id_kilo, 1], 
#        z = spdf.temp_hec@data$wl[id_kilo] + 3, 
#        label = spdf.temp_hec@data$M100[id_kilo], 
#        col = "black", cex = 1, font = 1, add = TRUE)
# 
# scatter3D(x = coordinates(spdf.gs)[, 2],
#           y = - coordinates(spdf.gs)[, 1],
#           z = c(wl_r, wl_d),
#           type = "p", pch = 21, col = "black", bg = c("white", "black"),
#           cex = 1.2, add = TRUE)
# text3D(x = coordinates(spdf.gs)[, 2],
#        y = - coordinates(spdf.gs)[, 1],
#        z = c(wl_r, wl_d) + 10,
#        label = spdf.gs$gauging_station, 
#        col = "black", cex = 1.5, font = 2, add = TRUE)
# dev.off()


#####
# 3D
##
# import and convert raster data
# dem
raster.dem <- raster("data-raw/raster.dem_dessau.tif")
raster.dem <- crop(raster.dem, ext)
raster.dem <- mask(raster.dem, spdf.mask_s)

# aggregate to decrease resolution
raster.dem <- aggregate(raster.dem, 5)

# get rid of some dem artefacts
raster.dem[raster.dem > 62] <- 62

df.dem <- as.data.frame(raster.dem, xy = TRUE, na.rm = FALSE)
ma.dem <- as.matrix(raster.dem)

# csa
raster.csa <- raster("data-raw/raster.csa_dessau.tif")
raster.csa <- crop(raster.csa, ext)
raster.csa <- mask(raster.csa, spdf.mask_s)

# aggregate to decrease resolution
raster.csa <- aggregate(raster.csa, 5, median)

df.csa <- as.data.frame(raster.csa, xy = TRUE, na.rm = FALSE)
ma.csa <- as.matrix(raster.csa)

ma.wl <- ma.dem
raster.wl <- raster(raster.dem)
for (s in station_int) {
    ma.wl[ma.csa == s] <- wldf$w[wldf$station_int == s]
    raster.wl[raster.csa == s] <- wldf$w[wldf$station_int == s]
}

ma.wl[ma.wl <= ma.dem] <- NA
raster.wl[raster.wl <= raster.dem] <- NA


pdf(file = paste0(out_dir, "/04_3D_5m.pdf"), width = 15, height = 5)
# dem
persp3D(x = unique(df.dem$y), y = - unique(df.dem$x), z = ma.dem, 
        col = dem_colfunc((62 - 50)*2), 
        # clim = c(50, 62),
        # colkey = list(length = 0.3,
        #               width = 0.4,
        #               shift = 0.2), 
        # clab = c("m a.s.l.", "", "DEM"),
        zlim = c(zmin, zmax), expand = 10, box = FALSE,
        scale = FALSE, plot = TRUE, phi = 10, theta = theta,
        lighting = TRUE, lphi = lphi, ltheta = ltheta, shade = 0.5)

# water level
persp3D(x = unique(df.dem$y), y = - unique(df.dem$x), z = ma.wl,
        add = TRUE,
        col = add.alpha(wl_colfunc(20), 0.5),
        clim = c(54, 60.6),
        colkey = list(length = 0.3,
                      width = 0.4,
                      shift = -0.2),
        clab = c("water level"))

# hectometer
id_kilo <- which(spdf.temp_hec@data$M100 %in% c(258, 259, 260, 261))
# c(258, 258.5, 259, 259.5, 260, 260.5, 261, 261.5)
scatter3D(x = coordinates(spdf.temp_hec)[id_kilo, 2], 
          y = - coordinates(spdf.temp_hec)[id_kilo, 1], 
          z = spdf.temp_hec@data$wl[id_kilo], 
          type = "p", pch = 21, col = "black", bg = "grey", cex = 0.7, 
          add = TRUE)
# text3D(x = coordinates(spdf.temp_hec)[id_kilo, 2], 
#        y = - coordinates(spdf.temp_hec)[id_kilo, 1], 
#        z = spdf.temp_hec@data$wl[id_kilo] + 3, 
#        label = spdf.temp_hec@data$M100[id_kilo], 
#        col = "white", cex = 1, font = 2, add = TRUE)
# text3D(x = coordinates(spdf.temp_hec)[id_kilo, 2], 
#        y = - coordinates(spdf.temp_hec)[id_kilo, 1], 
#        z = spdf.temp_hec@data$wl[id_kilo] + 3, 
#        label = spdf.temp_hec@data$M100[id_kilo], 
#        col = "black", cex = 1, font = 1, add = TRUE)

scatter3D(x = coordinates(spdf.gs)[, 2],
          y = - coordinates(spdf.gs)[, 1],
          z = c(wl_r, wl_d),
          type = "p", pch = 21, col = "black", bg = "black",
          cex = 1.5, add = TRUE)
# text3D(x = coordinates(spdf.gs)[, 2],
#        y = - coordinates(spdf.gs)[, 1],
#        z = c(wl_r, wl_d) + 10,
#        label = spdf.gs$gauging_station, 
#        col = "black", cex = 1.5, font = 2, add = TRUE)
dev.off()

#####
# overview
pdf(file = paste0(out_dir, "/06_top.pdf"), width = 13, height = 3)

# set layout 1:3:1
layout(matrix(c(1, 2, 3), 1, 3, byrow = TRUE), widths = c(2, 9, 2))

# plot w sequence
plot(x = df.gd_r$date, y = df.gd_r$wl, type = "l", ylim = c(54, 61), 
     xlab = "time", xaxt = "n", ylab = "water level (m a.s.l.)", yaxt = "n",
     col = 1, lty = 1, bty = "n") # main = "ROSSLAU", 
points(as.Date(h), wl_r, cex = 1, pch = 21, col = "darkblue", bg = "darkblue")
# axis.Date(1, at = as.Date(paste0(year, c("-02-01", "-03-01", "-04-01",
#                                          "-05-01", "-06-01", "-08-01", 
#                                          "-09-01", "-10-01", "-11-01",
#                                          "-12-01"))), 
#           tck = -0.02, labels = FALSE)
axis.Date(1, at = c(as.Date(paste0(year, "-01-01")), 
                    as.Date(paste0(year, "-07-01")), 
                    as.Date(paste0(year_p, "-01-01"))), format = "%y-%m-%d", 
          tck = -0.05, labels = TRUE)
axis(2, at = c(54, 56, 58, 60), tck = -0.05, labels = TRUE)


# plot hyd1d
plotShiny(wldf, FALSE, FALSE, FALSE,
          xlim = c(min(station_int)/1000, max(station_int)/1000),
          ylim = c(54, 61),
          xlab = "river kilometer", yaxt = "n", ylab = NA) #, ylab = "water level (m a.s.l.)")
axis(2, at = c(54, 56, 58, 60), tck = -0.05, labels = TRUE)

# plot w sequence
plot(x = df.gd_d$date, y = df.gd_d$wl, type = "l", ylim = c(54, 61), 
     xlab = "time", xaxt = "n", yaxt = "n", ylab = NA, # ylab = "water level (m a.s.l.)",
     col = 1, lty = 1, bty = "n") #, main = "DESSAU"
points(as.Date(h), wl_d, cex = 1, pch = 21, col = "darkblue", bg = "darkblue")
# axis.Date(1, at = as.Date(paste0(year, c("-02-01", "-03-01", "-04-01",
#                                          "-05-01", "-06-01", "-08-01", 
#                                          "-09-01", "-10-01", "-11-01",
#                                          "-12-01"))), 
#           tck = -0.02, labels = FALSE)
axis.Date(1, at = c(as.Date(paste0(year, "-01-01")), 
                    as.Date(paste0(year, "-07-01")), 
                    as.Date(paste0(year_p, "-01-01"))), format = "%y-%m-%d", 
          tck = -0.05, labels = TRUE)
axis(2, at = c(54, 56, 58, 60), tck = -0.05, labels = TRUE)

# close plotting device
dev.off()

# quit R
#q("no")

