################################################################################
# movie_jpg_lenzen.R
#
# author: arnd.weber@bafg.de
# date:   01.08.2018
#
# purpose:
#   - plot a sequence of 3D plots with water level data near Dessau
#   - combine this sequence to an mpeg4-video using 'movie_mp4.R'
#
################################################################################

#####
# argument input
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) {
    stop("Three argument must be supplied (date_min, date_max, id)!\n",
        call. = FALSE)
} else {
    date_min <- as.POSIXct(args[1])
    date_max <- as.POSIXct(args[2])
    j <- as.numeric(args[3])
    year <- strftime(date_min, "%Y")
    year_p <- as.character(as.numeric(year) + 1)
}

# # testing
# date_min <- as.POSIXct("2013-01-01")
# date_max <- as.POSIXct("2013-12-31")
# j <- 1
# year <- strftime(date_min, "%Y")
# year_p <- as.character(as.numeric(year) + 1)

#####
# 3D setting
phi <- 25
theta <- -170 #-70 #-80
lphi <- 120
ltheta <- 30
zmin <- -200
zmax <- 25

#####
# configure output
verbose <- TRUE
quiet <- !verbose

# output paths
#out_dir <- paste0("vignettes/movie/", year,"/")
out_dir <- paste0("vignettes/movie/lenzen/", year,"/")
dir.create(out_dir, verbose, TRUE)

#####
# load the packages
require(sp)
require(raster)
require(rgdal)
require(plot3D)
require(hyd1d)
require(plotrix)

#####
# convert tif files
# raster.dem <- raster("data-raw/raster.dem_lenzen.tif")
# writeRaster(raster.dem, filename = "data-raw/raster.dem_lenzen_tmp.tif", 
#             overwrite=TRUE)
# raster.csa <- raster("data-raw/raster.csa_lenzen.tif")
# writeRaster(raster.csa, filename = "data-raw/raster.csa_lenzen_tmp.tif")

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
ext <- extent(263500, 268800, 5882500, 5886800)

##
# import and convert raster data
raster.dem <- raster("data-raw/raster.dem_lenzen.tif")
raster.dem <- raster::crop(raster.dem, ext)
raster.dem <- aggregate(raster.dem, fact = 5, fun = mean, na.rm = TRUE)

# get rid of some dem artefacts
#raster.dem[raster.dem > 62] <- 62

df.dem <- as.data.frame(raster.dem, xy = TRUE, na.rm = FALSE)
ma.dem <- as.matrix(raster.dem)

raster.csa <- raster("data-raw/raster.csa_lenzen.tif")
raster.csa <- raster::crop(raster.csa, ext)
raster.csa <- aggregate(raster.csa, fact = 5, fun = modal, na.rm = TRUE)
dataType(raster.csa) <- "INT4U"

df.csa <- as.data.frame(raster.csa, xy = TRUE, na.rm = FALSE)
ma.csa <- as.matrix(raster.csa)

##
# import hectometer
spdf.hectometer <- readOGR("data-raw", "doc_hectometer_lenzen", 
                           verbose = verbose, pointDropZ = TRUE)
spdf.hectometer <- crop(spdf.hectometer, ext)
sp.hectometer <- SpatialPoints(spdf.hectometer, 
                               proj4string = crs(raster.dem))

##
# import csa polygons
spdf.csa <- readOGR("data-raw", "doc_csa_lenzen", 
                    verbose = verbose, pointDropZ = TRUE)
spdf.csa <- crop(spdf.csa, ext)

df.csa_borders <- data.frame(x0 = numeric(), y0 = numeric(), z0 = numeric(), 
                             x1 = numeric(), y1 = numeric(), z1 = numeric())

for (k in 1:nrow(spdf.csa)) {
    # convert SpatialPolygonsDataFrame to matrix with coordinates
    ma.polygon <- spdf.csa@polygons[[k]]@Polygons[[1]]@coords
    
    # construct df to collect coordinate results
    n_coords <- nrow(ma.polygon) - 2
    df.csa_borders_temp <- data.frame(x0 = rep(as.numeric(NA), n_coords), 
                                      y0 = rep(as.numeric(NA), n_coords), 
                                      z0 = rep(zmin + 0.001, n_coords), 
                                      x1 = rep(as.numeric(NA), n_coords), 
                                      y1 = rep(as.numeric(NA), n_coords), 
                                      z1 = rep(zmin + 0.001, n_coords))
    
    for (l in 1:n_coords) {
        df.csa_borders_temp$x0[l] <- ma.polygon[l, "x"]
        df.csa_borders_temp$y0[l] <- ma.polygon[l, "y"]
        df.csa_borders_temp$x1[l] <- ma.polygon[l + 1, "x"]
        df.csa_borders_temp$y1[l] <- ma.polygon[l + 1, "y"]
    }
    
    df.csa_borders <- rbind(df.csa_borders, df.csa_borders_temp)
}

#####
# wldf
station_int <- as.integer(unique(raster.csa))
wldf_template <- WaterLevelDataFrame(river = "Elbe", time = as.POSIXct(NA),
                                     station_int = station_int)

#####
# loop over a sequence of 1 year
##
# create the sequence
seq <- seq(date_min, date_max, "days")

# subset df.gauging_data for w plotting
load(paste0(lib, "hyd1d/data/df.gauging_data_latest.rda"))
id <- which(df.gauging_data$gauging_station == "LENZEN" &
                df.gauging_data$date >= as.Date(date_min) &
                df.gauging_data$date <= as.Date(date_max))
df.gd <- df.gauging_data[id,]
id_gs <- which(df.gauging_station_data$gauging_station == "LENZEN")
pnp <- df.gauging_station_data$pnp[id_gs]
mw <- df.gauging_station_data$mw[id_gs] + pnp
df.gd$wl <- pnp + df.gd$w/100

##
# loop
# j <- 1
for (h in seq[j]) {
    
    i <- as.POSIXct.numeric(h, tz = "CET", origin = "1970-01-01 00:00:00")
    
    out_file <- paste0(out_dir, "flood3_", sprintf("%03d", j), ".jpg")
    if (file.exists(out_file)) {
        # write output
        write(paste0(strftime(i, "%d.%m.%Y"), ": ", out_file,
                     " existiert bereits"), stdout())
        
        # increase counter j
        j <- j + 1
        
        next
    } else {
        # write output
        write(strftime(i, "%d.%m.%Y"), stdout())
    }
    
    # compute a water level
    wldf <- wldf_template
    setTime(wldf) <- i
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
    ma.wl_contour <- ma.wl
    ma.wl_contour[!is.na(ma.wl_contour)] <- zmin
    
    # query wl at hectometers a
    spdf.temp <- raster::extract(x = raster.wl, 
                                 y = sp.hectometer, 
                                 sp = TRUE)
    spdf.temp_hec <- spdf.hectometer
    spdf.temp_hec@data <- cbind(spdf.temp_hec@data, spdf.temp@data$layer)
    names(spdf.temp_hec@data)[length(names(spdf.temp_hec@data))] <- "wl"
    
    # open plotting device and export to jpg with resolution of 1920 × 1080
    jpeg(filename = out_file, width = 1920, height = 1080, units = "px", 
         pointsize = 20)
    
    # set layout 1:3
    layout(matrix(c(1, 2), 1, 2, byrow = TRUE), widths = c(1, 3))
    par(font.lab = 2)
    
    # plot w sequence
    plot(x = df.gd$date, y = df.gd$wl, 
         type = "l", ylim = c(13.5, 21), xlab = NA, xaxt = "n",
         ylab = "W am Pegel Lenzen (m über NHN (DHHN92))",
         col = "darkblue")
    abline(h = mw, lty = 3, col = 1)
    boxed.labels(as.Date(paste0(year, "-02-15")), mw, "MW", cex = 0.8, 
                 border = FALSE)
    points(as.Date(i), df.gd$wl[which(df.gd$date == as.Date(i))], 
           cex = 1, pch = 21, col = "darkblue", bg = "darkblue")
    axis.Date(1, at = as.Date(paste0(year, c("-02-01", "-03-01", "-04-01",
                                             "-05-01", "-06-01", "-08-01", 
                                             "-09-01", "-10-01", "-11-01",
                                             "-12-01"))), 
              tck = -0.02, labels = FALSE)
    axis.Date(1, at = c(as.Date(paste0(year, "-01-01")), 
                        as.Date(paste0(year, "-07-01")), 
                        as.Date(paste0(year_p, "-01-01"))), format = "%d.%m.", 
              tck = -0.05, labels = TRUE)
    axis(2, at = c(15, 17, 19), tck = -0.02, labels = FALSE)
    box()
    
    # plot3D
    # dem
    persp3D(x = unique(df.dem$y), y = - unique(df.dem$x), z = ma.dem, 
            col = dem_colfunc((24 - 9)*2), 
            clim = c(9, 24),
            colkey = list(length = 0.3,
                          width = 0.4,
                          shift = 0.2), 
            clab = c("m über NHN", "(DHHN92)", "", "DGM"),
            zlim = c(zmin, zmax), expand = 10, box = FALSE,
            scale = FALSE, plot = TRUE, phi = phi, theta = theta,
            lighting = TRUE, lphi = lphi, ltheta = ltheta, shade = 0.5)
    # water level
    persp3D(x = unique(df.dem$y), y = - unique(df.dem$x), z = ma.wl,
            add = TRUE,
            col = add.alpha(wl_colfunc(20), 0.5),
            clim = c(13.5, 21),
            colkey = list(length = 0.3,
                          width = 0.4,
                          shift = -0.2),
            clab = c("Wasserspiegel"))
    persp3D(x = unique(df.dem$y), y = - unique(df.dem$x), z = ma.wl_contour,
            add = TRUE,
            colvar = ma.wl, 
            col = wl_colfunc(20), 
            colkey = FALSE,
            clim = c(13, 22))
    # water level contours
    segments3D(x0 = df.csa_borders$y0, y0 = -df.csa_borders$x0, 
               z0 = df.csa_borders$z0, x1 = df.csa_borders$y1, 
               y1 = -df.csa_borders$x1, z1 = df.csa_borders$z1, 
               add = TRUE, lty = 1, lwd = 1, col = "white")
    # hectometer
    id_kilo <- which(spdf.temp_hec@data$M100 == 476.5 |
                     spdf.temp_hec@data$M100 == 477 |
                     spdf.temp_hec@data$M100 == 477.5 |
                     spdf.temp_hec@data$M100 == 478 |
                     spdf.temp_hec@data$M100 == 478.5 |
                     spdf.temp_hec@data$M100 == 479 |
                     spdf.temp_hec@data$M100 == 479.5 |
                     spdf.temp_hec@data$M100 == 480 |
                     spdf.temp_hec@data$M100 == 480.5 |
                     spdf.temp_hec@data$M100 == 481 |
                     spdf.temp_hec@data$M100 == 481.5 |
                     spdf.temp_hec@data$M100 == 482 |
                     spdf.temp_hec@data$M100 == 482.5 | 
                     spdf.temp_hec@data$M100 == 483)
    id_kilo_text <- which(spdf.temp_hec@data$M100 == 477 |
                          spdf.temp_hec@data$M100 == 478 |
                          spdf.temp_hec@data$M100 == 479 |
                          spdf.temp_hec@data$M100 == 480 |
                          spdf.temp_hec@data$M100 == 481 |
                          spdf.temp_hec@data$M100 == 482 |
                          spdf.temp_hec@data$M100 == 483)
    scatter3D(x = coordinates(spdf.temp_hec)[id_kilo, 2], 
              y = - coordinates(spdf.temp_hec)[id_kilo, 1], 
              z = spdf.temp_hec@data$wl[id_kilo], 
              type = "p", pch = 21, col = "black", bg = "grey", cex = 1.5, 
              add = TRUE)
    scatter3D(x = coordinates(spdf.temp_hec)[-id_kilo, 2], 
              y = - coordinates(spdf.temp_hec)[-id_kilo, 1], 
              z = spdf.temp_hec@data$wl[-id_kilo], 
              type = "p", pch = 21, col = "black", bg = "grey", cex = 0.8, 
              add = TRUE)
    text3D(x = coordinates(spdf.temp_hec)[id_kilo_text, 2], 
           y = - coordinates(spdf.temp_hec)[id_kilo_text, 1], 
           z = spdf.temp_hec@data$wl[id_kilo_text] + 5, 
           label = spdf.temp_hec@data$M100[id_kilo_text], 
           col = "black", cex = 0.7, font = 2,
           add = TRUE)
    # hectometer in contours
    scatter3D(x = coordinates(spdf.temp_hec)[id_kilo, 2], 
              y = - coordinates(spdf.temp_hec)[id_kilo, 1], 
              z = rep(zmin + 0.001, length(id_kilo)), 
              type = "p", pch = 21, col = "black", bg = "grey", cex = 1.5, 
              add = TRUE)
    scatter3D(x = coordinates(spdf.temp_hec)[-id_kilo, 2], 
              y = - coordinates(spdf.temp_hec)[-id_kilo, 1], 
              z = rep(zmin + 0.001, length(spdf.temp_hec@data$wl[-id_kilo])), 
              type = "p", pch = 21, col = "black", bg = "grey", cex = 0.8, 
              add = TRUE)
    text3D(x = coordinates(spdf.temp_hec)[id_kilo_text, 2], 
           y = - coordinates(spdf.temp_hec)[id_kilo_text, 1], 
           z = rep(zmin + 0.001, length(id_kilo_text)) + 5, 
           label = spdf.temp_hec@data$M100[id_kilo_text], 
           col = "black", cex = 0.7, font = 2,
           add = TRUE)
    
    # title
    title(strftime(i, "%d.%m.%Y"), outer = TRUE, line = -2, font = 1)
    
    # close plotting device
    dev.off()
    
    # increase counter j
    j <- j + 1
    
}

# quit R
q("no")

