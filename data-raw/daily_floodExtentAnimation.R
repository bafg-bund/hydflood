##################################################
# daily_floodExtentAnimation.R
#
# author: arnd.weber@bafg.de
# date:   23.07.2019
#
# purpose: 
#   - compute daily flood extent for Dessau and Lenzen
#   - keep plotting with raster due to layout changes
#
##################################################

write("floodExtents for Dessau and Lenzen will be computed", stdout())

# load hyd1d
library(hyd1d)
library(hydflood)
library(raster)

# if (updateGaugingData(Sys.Date() - 10)) {
#     write("", stdout())
#     write("Forced an additional update of 'df.gauging_data'", stdout())
#     write("", stdout())
# }

# setwd
setwd(Sys.getenv("hydflood")) 

# temporal sequence (last X days) Sys.Date() - 8
dates <- as.character(seq.Date(as.Date("2015-01-01"), Sys.Date() - 2,
                               by = "1 day"))

#####
# gauging_station_data
wgs84 <- st_crs("EPSG:4326")
crs <- st_crs("EPSG:25833")
df.gsd <- na.omit(df.gauging_station_data)
sf.gsd <- st_as_sf(df.gsd, coords = c("longitude", "latitude"), crs = wgs84)
sf.gsd_D <- sf.gsd[which(sf.gsd$gauging_station %in% c("ROSSLAU", "DESSAU")), ]
sf.gsd_D <- st_transform(sf.gsd_D, crs)
sf.gsd_L <- sf.gsd[which(sf.gsd$gauging_station %in% c("SCHNACKENBURG",
                                                       "LENZEN")), ]
sf.gsd_L <- st_transform(sf.gsd_L, crs)

#####
# color function
dem_colfunc <- colorRampPalette(c("saddlebrown", "yellow", "darkgreen"))

#####
# DESSAU
ext_D <- ext(306050, 311870, 5747870, 5752220)
dem <- rast("data-raw/raster.dem_dessau.tif")
dem_plot <- dem
dem_plot[dem_plot > 62] <- 62
dem_plot[dem_plot < 50] <- 50
csa <- rast("data-raw/raster.csa_dessau.tif")
x <- hydSpatRaster(filename_dem = "data-raw/raster.dem_dessau.tif",
                   filename_csa = "data-raw/raster.csa_dessau.tif")
# mask <- st_read(dsn = "data-raw", layer = "doc_mask_dessau")
# mask <- st_transform(mask, wgs84)
# mask$SHAPE_Leng <- NULL
# mask$SHAPE_Area <- NULL
# mask_floodplain <- st_read(dsn = "data-raw", layer="doc_mask_dessau_floodplain")
# mask_floodplain <- st_transform(mask_floodplain, wgs84)
# mask_floodplain$SHAPE_Leng <- NULL
# mask_floodplain$SHAPE_Area <- NULL
# mask_floodplain <- st_intersection(mask_floodplain, mask)
# extent_wgs84 <- ext(mask)

# export a dem plot
dem <- paste0("/home/WeberA/freigaben/U/U3/Auengruppe_INFORM/EL_000_586_UFD/da",
              "ta/png/flood3_daily/DESSAU/dem.png")
if (!file.exists(dem)) {
    png(filename = dem, width = 960, height = 640, units = "px")
    plot(raster(dem_plot), col = dem_colfunc((62 - 50)*2), xlim = c(305000, 313000),
         legend.width = 1, horizontal = TRUE, bty = "n",
         legend.args = list(text = "elevation (m)"),
         xaxp = c(306000, 312000, 3), yaxp = c(5748000, 5752000, 2))
    # plot(dem_plot, col = dem_colfunc((62 - 50)*2), xlim = c(305000, 313000),
    #      bty = "n", xaxp = c(306000, 312000, 3), yaxp = c(5748000, 5752000, 2),
    #      plg = list(title = "elevation (m)", horiz = TRUE, width = 1))
    plot(sf.gsd_D$geometry, pch = 21, bg = "white", add =TRUE)
    text(st_coordinates(sf.gsd_D[1, ]), pos = 3,
         labels = sf.gsd_D$gauging_station[1])
    text(st_coordinates(sf.gsd_D[2, ]), pos = 1,
         labels = sf.gsd_D$gauging_station[2])
    dev.off()
}

# loop over all dates
for (a_date in dates) {
    
    write(paste0("DESSAU: ", a_date), stdout())
    
    f_out <- paste0("/home/WeberA/freigaben/U/U3/Auengruppe_INFORM/EL_000_586_",
                    "UFD/data/png/flood3_daily/DESSAU/flood3_",
                    gsub("-", "", a_date), ".png")
    
    if (file.exists(f_out)) {
        write("  exists already", stdout())
    } else {
        write("  will be computed", stdout())
        
        # compute flood extent
        flood_extent <- tryCatch({
            flood_extent <- flood3(x, as.Date(a_date))
            flood_extent[flood_extent == 0] <- NA
        },
        error = function(cond) {NA})
        
        # plotting with raster functions
        if (!is.na(flood_extent)) {
            png(filename = f_out, width = 960, height = 640, units = "px")
            plot(raster(dem_plot), col = dem_colfunc((62 - 50)*2),
                 xlim = c(305000, 313000), legend.width = 1, horizontal = TRUE,
                 bty = "n", legend.args = list(text = "elevation (m)"),
                 xaxp = c(306000, 312000, 3), yaxp = c(5748000, 5752000, 2))
            # plot(dem_plot, col = dem_colfunc((62 - 50)*2), xlim = c(305000, 313000),
            #      bty = "n", xaxp = c(306000, 312000, 3), yaxp = c(5748000, 5752000, 2),
            #      plg = list(title = "elevation (m)", horiz = TRUE, width = 1))
            plot(raster(flood_extent), col = "blue", add = TRUE, legend = FALSE)
            plot(sf.gsd_D$geometry, pch = 21, bg = "white", add =TRUE)
            text(st_coordinates(sf.gsd_D[1, ]), pos = 3,
                 labels = sf.gsd_D$gauging_station[1])
            text(st_coordinates(sf.gsd_D[2, ]), pos = 1,
                 labels = sf.gsd_D$gauging_station[2])
            dev.off()
        }
    }
}

#####
# LENZEN
ext_L <- ext(261940, 270870, 5881635, 5887550)
dem <- rast("data-raw/raster.dem_lenzen.tif")
dem_plot <- dem
dem_plot[dem_plot > 24] <- 24
dem_plot[dem_plot < 9] <- 9
csa <- rast("data-raw/raster.csa_lenzen.tif")
x <- hydSpatRaster(filename_dem = "data-raw/raster.dem_lenzen.tif",
                   filename_csa = "data-raw/raster.csa_lenzen.tif")

# mask <- st_read(dsn = "data-raw", layer = "doc_mask_lenzen")
# mask <- st_transform(mask, wgs84)
# mask$SHAPE_Leng <- NULL
# mask$SHAPE_Area <- NULL
# mask_floodplain <- st_read(dsn = "data-raw", layer="doc_mask_lenzen_floodplain")
# mask_floodplain <- st_transform(mask_floodplain, wgs84)
# mask_floodplain$SHAPE_Leng <- NULL
# mask_floodplain$SHAPE_Area <- NULL
# mask_floodplain <- st_intersection(mask_floodplain, mask)
# extent_wgs84 <- ext(mask)

# export a dem plot
dem <- paste0("/home/WeberA/freigaben/U/U3/Auengruppe_INFORM/EL_000_586_UFD/da",
              "ta/png/flood3_daily/LENZEN/dem.png")
if (!file.exists(dem)) {
    png(filename = dem, width = 960, height = 640, units = "px")
    plot(raster(dem_plot), col = dem_colfunc((24 - 9)*2), xlim = c(263500, 268800),
         legend.width = 1, horizontal = TRUE, bty = "n",
         legend.args = list(text = "elevation (m)"),
         xaxp = c(264000, 268000, 2), yaxp = c(5884000, 5886000, 1))
    # plot(dem_plot, col = dem_colfunc((24 - 9)*2), xlim = c(263500, 268800),
    #      bty = "n", xaxp = c(264000, 268000, 2), yaxp = c(5884000, 5886000, 1),
    #      plg = list(title = "elevation (m)", horiz = TRUE, width = 1))
    # plot(sf.gsd_L$geometry, pch = 21, bg = "white", add =TRUE)
    # text(st_coordinates(sf.gsd_L[1, ]), pos = 3,
    #      labels = sf.gsd_L$gauging_station[1])
    # text(st_coordinates(sf.gsd_L[2, ]), pos = 1,
    #      labels = sf.gsd_L$gauging_station[2])
    dev.off()
}

# loop over all dates
for (a_date in dates) {
    
    write(paste0("LENZEN: ", a_date), stdout())
    
    f_out <- paste0("/home/WeberA/freigaben/U/U3/Auengruppe_INFORM/EL_000_586_",
                    "UFD/data/png/flood3_daily/LENZEN/flood3_",
                    gsub("-", "", a_date), ".png")
    
    if (file.exists(f_out)) {
        write("  exists already", stdout())
    } else {
        write("  will be computed", stdout())
        
        # compute flood extent
        flood_extent <- tryCatch({
            flood_extent <- flood3(x, as.Date(a_date))
            flood_extent[flood_extent == 0] <- NA
        },
        error = function(cond) {NA})
        
        # plotting with raster functions
        if (!is.na(flood_extent)) {
            png(filename = f_out, width = 960, height = 640, units = "px")
            plot(raster(dem_plot), col = dem_colfunc((24 - 9)*2), xlim = c(263500, 268800),
                 legend.width = 1, horizontal = TRUE, bty = "n",
                 legend.args = list(text = "elevation (m)"),
                 xaxp = c(264000, 268000, 2), yaxp = c(5884000, 5886000, 1))
            # plot(dem_plot, col = dem_colfunc((24 - 9)*2), xlim = c(263500, 268800),
            #      bty = "n", xaxp = c(264000, 268000, 2), yaxp = c(5884000, 5886000, 1),
            #      plg = list(title = "elevation (m)", horiz = TRUE, width = 1))
            plot(raster(flood_extent), col = "blue", add = TRUE, legend = FALSE)
            # plot(sf.gsd_L$geometry, pch = 21, bg = "white", add =TRUE)
            # text(st_coordinates(sf.gsd_L[1, ]), pos = 3,
            #      labels = sf.gsd_L$gauging_station[1])
            # text(st_coordinates(sf.gsd_L[2, ]), pos = 1,
            #      labels = sf.gsd_L$gauging_station[2])
            dev.off()
        }
    }
}

##########
# ElBiota-Areas
# dates <- as.character(seq.Date(as.Date("2020-01-01"), Sys.Date() - 2,
#                                by = "1 day"))
if (require("ElBiota")) {
    
    # loop over areas
    for (an_area in spdf.areas_sel$mapset) {
        
        print(an_area)
        
        spdf.area <- spdf.areas_sel[which(spdf.areas_sel$mapset == an_area), ]
        sac <- spdf.sac[which(spdf.sac$mapset == an_area), ]
        
        if (! file.exists(paste0("data-raw/raster.dem_", an_area, ".tif"))) {
            r <- getDEM(paste0("data-raw/raster.dem_", an_area, ".tif"),
                        ext = ext(spdf.area), crs = crs)
        } else {
            r <- rast(paste0("data-raw/raster.dem_", an_area, ".tif"))
        }
        
        x <- hydSpatRaster(filename_dem = paste0("data-raw/raster.dem_",
                                                 an_area, ".tif"),
                           filename_csa = paste0("data-raw/raster.csa_",
                                                 an_area, ".tif"))
        
        # export a dem plot
        dir.create(paste0("/home/WeberA/freigaben/U/U3/Auengruppe_INFORM/EL_00",
                          "0_586_UFD/data/png/flood3_daily/", an_area),
                   FALSE, TRUE)
        dem <- paste0("/home/WeberA/freigaben/U/U3/Auengruppe_INFORM/EL_000_58",
                      "6_UFD/data/png/flood3_daily/", an_area, "/dem.png")
        if (!file.exists(dem)) {
            png(filename = dem, width = 960, height = 640, units = "px")
            plot(raster(r), col = dem_colfunc((24 - 9)*2), legend.width = 1,
                 horizontal = TRUE, bty = "n",
                 legend.args = list(text = "elevation (m)"))
            # plot(r, col = dem_colfunc((24 - 9)*2), bty = "n",
            #      plg = list(title = "elevation (m)", horiz = TRUE, width = 1))
            points(sac, pch = 21, bg = "white", cex = 0.8)
            text(sac, pos = 3, labels = sac$plot_id, cex = 0.8)
            dev.off()
        }
        
        # loop over all dates
        for (a_date in dates) {
            
            write(paste0(an_area, ": ", a_date), stdout())
            
            f_out <- paste0("/home/WeberA/freigaben/U/U3/Auengruppe_INFORM/EL_",
                            "000_586_UFD/data/png/flood3_daily/", an_area,
                            "/flood3_", gsub("-", "", a_date), ".png")
            
            if (file.exists(f_out)) {
                write("  exists already", stdout())
            } else {
                write("  will be computed", stdout())
                
                # compute flood extent
                flood_extent <- tryCatch({
                    flood_extent <- flood3(x, as.Date(a_date))
                    flood_extent[flood_extent == 0] <- NA
                },
                error = function(cond) {NA})
                
                # plotting with raster functions
                    if (!is.na(flood_extent)) {
                    png(filename = f_out, width = 960, height = 640, 
                        units = "px")
                    plot(raster(r), col = dem_colfunc((24 - 9)*2),
                         legend.width = 1, horizontal = TRUE, bty = "n",
                         legend.args = list(text = "elevation (m)"))
                    # plot(r, col = dem_colfunc((24 - 9)*2), bty = "n",
                    #      plg = list(title = "elevation (m)", horiz = TRUE,
                    #                 width = 1))
                    plot(raster(flood_extent), col = "blue", add = TRUE,
                         legend = FALSE)
                    points(sac, pch = 21, bg = "white", cex = 0.8)
                    text(sac, pos = 3, labels = sac$plot_id, cex = 0.8)
                    dev.off()
                }
            }
        }
    }
}



#
q("no")
