################################################################################
# movie_jpg.R
#
# author: arnd.weber@bafg.de
# date:   26.07.2018
#
# purpose:
#   - plot a sequence of 3D plots with water level data near Dessau
#   - combine this sequence to an mpeg4-video using 'movie_mp4.R'
#
################################################################################
if (length(args) != 3) {
    stop("Three argument must be supplied (date_min, date_max, id)!\n", 
         call. = FALSE)
} else {
    date_min <- as.POSIXct(args[1])
    date_max <- as.POSIXct(args[2])
    j <- as.numeric(args[3])
}

# configure output
verbose <- TRUE
quiet <- !verbose

# standard library path for the package install
R_version <- paste(sep = ".", R.Version()$major, R.Version()$minor)
lib <- paste0("~/R/", R_version, "/")

# output paths
out_dir <- paste0("vignettes/movie/", strftime(date_min, "%Y"),"/")
dir.create(out_dir, verbose, TRUE)

#####
# load the packages
require(sp, lib.loc = lib)
require(raster, lib.loc = lib)
require(rgdal, lib.loc = lib)
require(plot3D, lib.loc = lib)
require(hyd1d, lib.loc = lib)
require(plotrix, lib.loc = lib)

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
ext <- extent(309000, 310000, 5749000, 5750000)
#ext <- extent(309200, 309700, 5749600, 5749800)

##
# import and convert raster data
#raster.dem <- raster("data-raw/raster.dem_dessau.tif")
raster.dem <- raster("data-raw/raster.dem.tif")
raster.dem <- raster::crop(raster.dem, ext)

# get rid of some dem artefacts
raster.dem[raster.dem > 65] <- 65

df.dem <- as.data.frame(raster.dem, xy = TRUE, na.rm = FALSE)
ma.dem <- as.matrix(raster.dem)

#raster.csa <- raster("data-raw/raster.csa_dessau.tif")
raster.csa <- raster("data-raw/raster.csa.tif")
raster.csa <- raster::crop(raster.csa, ext)

df.csa <- as.data.frame(raster.csa, xy = TRUE, na.rm = FALSE)
ma.csa <- as.matrix(raster.csa)

##
# import hectometer
spdf.hectometer <- readOGR("data-raw", "doc_hectometer_dessau", 
                           verbose = verbose, pointDropZ = TRUE)
spdf.hectometer <- crop(spdf.hectometer, ext)
sp.hectometer <- SpatialPoints(spdf.hectometer, 
                               proj4string = crs(raster.dem))

#####
# wldf
station_int <- unique(raster.csa)
wldf_template <- WaterLevelDataFrame(river = "Elbe", time = as.POSIXct(NA),
                                     station_int = station_int)

#####
# 3D setting
phi <- 30
theta <- -60 #-80
lphi <- 120
ltheta <- 30

#####
# loop over a sequence of 1 year
##
# create the sequence
seq <- seq(date_min, date_max, "days")

# subset df.gauging_data for w plotting
load(paste0(lib, "hyd1d/data/df.gauging_data_latest.rda"))
id <- which(df.gauging_data$gauging_station == "DESSAU" &
                df.gauging_data$date >= as.Date(date_min) &
                df.gauging_data$date <= as.Date(date_max))
df.gd <- df.gauging_data[id,]
id_dessau <- which(df.gauging_station_data$gauging_station == "DESSAU")
pnp <- df.gauging_station_data$pnp[id_dessau]
mw <- df.gauging_station_data$mw[id_dessau] + pnp
df.gd$wl <- pnp + df.gd$w/100

##
# loop
#j <- 1
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
    
    # assemble countours for the surface3D plot of ma.wl
    wls <- as.numeric(levels(as.factor(ma.wl)))
    wls.contours <- numeric()
    for (a_wl in 1:length(wls)) {
        if (a_wl == 1) {
            wls.contours <- append(wls.contours, wls[a_wl] - 
                                                 (wls[a_wl + 1] - wls[a_wl])/2)
            wls.contours <- append(wls.contours, (wls[a_wl] + wls[a_wl + 1])/2)
        } else if (a_wl == length(wls)) {
            wls.contours <- append(wls.contours, wls[a_wl] + 
                                       (wls[a_wl] - wls[a_wl - 1])/2)
        } else {
            wls.contours <- append(wls.contours, (wls[a_wl] + wls[a_wl + 1])/2)
        }
    }
    
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
         type = "l", xlab = NA, 
         ylab = "W am Pegel Dessau (m über NHN (DHHN92))",
         col = "darkblue")
    abline(h = mw, lty = 3, col = 1)
    boxed.labels(as.Date("2002-11-15"), mw, "MW", cex = 0.8, border = FALSE)
    points(as.Date(i), df.gd$wl[which(df.gd$date == as.Date(i))], 
           cex = 1, pch = 21, col = "darkblue", bg = "darkblue")
    
    # plot3D
    persp3D(x = unique(df.dem$y), y = - unique(df.dem$x), z = ma.dem, 
            col = dem_colfunc(69 - 45), 
            clim = c(45, 69),
            colkey = list(length = 0.3,
                          width = 0.4,
                          shift = 0.2), 
            clab = c("m über NHN", "(DHHN92)", "", "DGM"),
            zlim = c(10, 69), expand = 10, box = FALSE,
            scale = FALSE, plot = TRUE, phi = phi, theta = theta,
            lighting = TRUE, lphi = lphi, ltheta = ltheta, shade = 0.5)
    persp3D(x = unique(df.dem$y), y = - unique(df.dem$x), z = ma.wl, 
            add = TRUE, 
            col = add.alpha(wl_colfunc(20), 0.5),
            colkey = list(length = 0.3,
                          width = 0.4,
                          shift = -0.2), 
            clab = c("Wasserspiegel"),
            image = list(side = "zmin"),
            contour = list(levels = wls.contours,
                           col = "white",
                           labels = NA))
    scatter3D(x = coordinates(spdf.temp_hec)[,2], 
              y = - coordinates(spdf.temp_hec)[,1], 
              z = spdf.temp_hec@data$wl, 
              type = "p", pch = 21, col = "black", bg = "grey", cex = 1.5, 
              add = TRUE)
    text3D(x = coordinates(spdf.temp_hec)[,2], 
           y = - coordinates(spdf.temp_hec)[,1], 
           z = spdf.temp_hec@data$wl + 2, 
           label = spdf.temp_hec@data$M100, 
           col = "grey", cex = 0.7, 
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

