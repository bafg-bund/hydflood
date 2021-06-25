##################################################
# daily_floodExtentAnimation.R
#
# author: arnd.weber@bafg.de
# date:   23.07.2019
#
# purpose: 
#   - compute daily flood extent for Dessau and Lenzen
#
##################################################

write("floodExtents for Dessau and Lenzen will be computed", stdout())

# load hyd1d
library(hyd1d)
library(hydflood)
options("rgdal_show_exportToProj4_warnings" = "none")
library(rgdal)

if (updateGaugingData(Sys.Date() - 10)) {
    write("", stdout())
    write("Forced an additional update of 'df.gauging_data'", stdout())
    write("", stdout())
}

# setwd
setwd(Sys.getenv("hydflood")) 

# temporal sequence (last X days) Sys.Date() - 8
dates <- as.character(seq.Date(as.Date("2015-01-01"), Sys.Date() - 2,
                               by = "1 day"))

#####
# gauging_station_data
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
crs <- CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")
df.gsd <- na.omit(df.gauging_station_data)
coordinates <- df.gsd[, c("longitude", "latitude")]
spdf.gsd <- SpatialPointsDataFrame(coords = coordinates, data = df.gsd,
                                   proj4string = wgs84)
spdf.gsd_D <- spdf.gsd[which(spdf.gsd$gauging_station %in% c("ROSSLAU", "DESSAU")), ]
spdf.gsd_D <- spTransform(spdf.gsd_D, crs)
spdf.gsd_L <- spdf.gsd[which(spdf.gsd$gauging_station %in% c("SCHNACKENBURG", "LENZEN")), ]
spdf.gsd_L <- spTransform(spdf.gsd_L, crs)

#####
# color function
dem_colfunc <- colorRampPalette(c("saddlebrown", "yellow", "darkgreen"))

#####
# DESSAU
ext_D <- extent(306050, 311870, 5747870, 5752220)
dem <- raster("data-raw/raster.dem_dessau.tif")
crs(dem) <- crs
dem_plot <- dem
dem_plot[dem_plot > 62] <- 62
dem_plot[dem_plot < 50] <- 50
csa <- raster("data-raw/raster.csa_dessau.tif")
crs(csa) <- crs
x <- hydRasterStack(filename_dem = "data-raw/raster.dem_dessau.tif",
                    filename_csa = "data-raw/raster.csa_dessau.tif")
# mask <- readOGR(dsn="data-raw", layer="doc_mask_dessau")
# mask <- spTransform(mask, wgs84)
# mask_floodplain <- readOGR(dsn="data-raw", layer="doc_mask_dessau_floodplain")
# mask_floodplain <- spTransform(mask_floodplain, wgs84)
# mask_floodplain <- crop(mask_floodplain, mask)

# x <- sort(unique(mask@polygons[[1]]@Polygons[[1]]@coords[,1]))
# y <- sort(unique(mask@polygons[[1]]@Polygons[[1]]@coords[,2]))
# extent_wgs84 <- extent(x[2], x[3], y[2], y[3])

# export a dem plot
dem <- paste0("/home/WeberA/freigaben/U/U3/Auengruppe_INFORM/EL_000_586_UFD/da",
              "ta/png/flood3_daily/DESSAU/dem.png")
if (!file.exists(dem)) {
    png(filename = dem, width = 960, height = 640, units = "px")
    plot(dem_plot, col = dem_colfunc((62 - 50)*2), xlim = c(305000, 313000),
         legend.width = 1, horizontal = TRUE, bty = "n",
         legend.args = list(text = "elevation (m)"),
         xaxp = c(306000, 312000, 3), yaxp = c(5748000, 5752000, 2))
    points(spdf.gsd_D, pch = 21, bg = "white")
    text(spdf.gsd_D[1,], pos = 3, labels = spdf.gsd_D$gauging_station[1])
    text(spdf.gsd_D[2,], pos = 1, labels = spdf.gsd_D$gauging_station[2])
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
        flood_extent <- flood3(x, as.Date(a_date))
        flood_extent[flood_extent == 0] <- NA
        
        # plotting with raster functions
        png(filename = f_out, width = 960, height = 640, units = "px")
        plot(dem_plot, col = dem_colfunc((62 - 50)*2), xlim = c(305000, 313000),
             legend.width = 1, horizontal = TRUE, bty = "n",
             legend.args = list(text = "elevation (m)"),
             xaxp = c(306000, 312000, 3), yaxp = c(5748000, 5752000, 2))
        plot(flood_extent, col = "blue", add = TRUE, legend = FALSE)
        points(spdf.gsd_D, pch = 21, bg = "white")
        text(spdf.gsd_D[1,], pos = 3, labels = spdf.gsd_D$gauging_station[1])
        text(spdf.gsd_D[2,], pos = 1, labels = spdf.gsd_D$gauging_station[2])
        dev.off()
        
        # attempts with tmap
        # flood_extent_wgs84 <- projectRaster(flood_extent, crs = wgs84,
        #                                     method = "ngb")
        # tmap_options(
        #     max.raster = c(plot = flood_extent_wgs84@ncols * flood_extent_wgs84@nrows,
        #                    view = flood_extent_wgs84@ncols * flood_extent_wgs84@nrows))
        #
        # # plot flood extent
        # tm_shape(mask_floodplain, projection = wgs84, bbox = extent_wgs84) +
        #     # fill for mask floodplain
        #     tm_fill(col = "white", alpha = 0.5) +
        # # airial image background
        # tm_view(alpha = 1, basemaps = "Esri.WorldImagery") +
        # # flood extent
        # tm_shape(flood_extent_wgs84) +
        #     tm_raster(col = "blue3", alpha = 1)
        #
        #
        # map <- tm_shape(mask_floodplain, projection = wgs84,
        #                 bbox = extent_wgs84) +
        #     tm_fill(col = "white", alpha = 0.5)
        #
        # map <- map + tm_view(alpha = 1, basemaps = "Esri.WorldImagery")
        # # gauging_stations
        # tm_markers()
        
    }
}

#####
# LENZEN
ext_L <- extent(261940, 270870, 5881635, 5887550)
dem <- raster("data-raw/raster.dem_lenzen.tif")
dem_plot <- dem
dem_plot[dem_plot > 24] <- 24
dem_plot[dem_plot < 9] <- 9
csa <- raster("data-raw/raster.csa_lenzen.tif")
x <- hydRasterStack(filename_dem = "data-raw/raster.dem_lenzen.tif",
                    filename_csa = "data-raw/raster.csa_lenzen.tif")

# mask <- readOGR(dsn="data-raw", layer="doc_mask_lenzen")
# mask_floodplain <- readOGR(dsn="data-raw", layer="doc_mask_lenzen_floodplain")
# mask_floodplain <- crop(mask_floodplain, mask)

# export a dem plot
dem <- paste0("/home/WeberA/freigaben/U/U3/Auengruppe_INFORM/EL_000_586_UFD/da",
              "ta/png/flood3_daily/LENZEN/dem.png")
if (!file.exists(dem)) {
    png(filename = dem, width = 960, height = 640, units = "px")
    plot(dem_plot, col = dem_colfunc((24 - 9)*2), xlim = c(263500, 268800),
         legend.width = 1, horizontal = TRUE, bty = "n",
         legend.args = list(text = "elevation (m)"),
         xaxp = c(264000, 268000, 2), yaxp = c(5884000, 5886000, 1))
    # points(spdf.gsd_L, pch = 21, bg = "white")
    # text(spdf.gsd_L[1,], pos = 3, labels = spdf.gsd_L$gauging_station[1])
    # text(spdf.gsd_L[2,], pos = 1, labels = spdf.gsd_L$gauging_station[2])
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
        flood_extent <- flood3(x, as.Date(a_date))
        flood_extent[flood_extent == 0] <- NA
        
        # plotting with raster functions
        png(filename = f_out, width = 960, height = 640, units = "px")
        plot(dem_plot, col = dem_colfunc((24 - 9)*2), xlim = c(263500, 268800),
             legend.width = 1, horizontal = TRUE, bty = "n",
             legend.args = list(text = "elevation (m)"),
             xaxp = c(264000, 268000, 2), yaxp = c(5884000, 5886000, 1))
        plot(flood_extent, col = "blue", add = TRUE, legend = FALSE)
        # points(spdf.gsd_L, pch = 21, bg = "white")
        # text(spdf.gsd_L[1,], pos = 3, labels = spdf.gsd_L$gauging_station[1])
        # text(spdf.gsd_L[2,], pos = 1, labels = spdf.gsd_L$gauging_station[2])
        dev.off()
        
        # attempts with tmap
        # flood_extent_wgs84 <- projectRaster(flood_extent, crs = wgs84,
        #                                     method = "ngb")
        # tmap_options(
        #     max.raster = c(plot = flood_extent_wgs84@ncols * flood_extent_wgs84@nrows,
        #                    view = flood_extent_wgs84@ncols * flood_extent_wgs84@nrows))
        #
        # # plot flood extent
        # tm_shape(mask_floodplain, projection = wgs84, bbox = extent_wgs84) +
        #     # fill for mask floodplain
        #     tm_fill(col = "white", alpha = 0.5) +
        # # airial image background
        # tm_view(alpha = 1, basemaps = "Esri.WorldImagery") +
        # # flood extent
        # tm_shape(flood_extent_wgs84) +
        #     tm_raster(col = "blue3", alpha = 1)
        #
        #
        # map <- tm_shape(mask_floodplain, projection = wgs84,
        #                 bbox = extent_wgs84) +
        #     tm_fill(col = "white", alpha = 0.5)
        #
        # map <- map + tm_view(alpha = 1, basemaps = "Esri.WorldImagery")
        # # gauging_stations
        # tm_markers()
        
    }
}

##########
# ElBiota-Areas
# dates <- as.character(seq.Date(as.Date("2020-01-01"), Sys.Date() - 2,
#                                by = "1 day"))
if (require("ElBiota")) {
    
    # loop over areas
    for (an_area in areas_esc) {
        
        print(an_area)
        
        spdf.area <- spdf.areas_sel[which(spdf.areas_sel$mapset == an_area), ]
        
        if (! file.exists(paste0("data-raw/raster.dem_", an_area, ".tif"))) {
            r <- getDEM(paste0("data-raw/raster.dem_", an_area, ".tif"),
                        ext = extent(spdf.area), crs = crs)
        } else {
            r <- raster(paste0("data-raw/raster.dem_", an_area, ".tif"))
        }
        
        x <- hydRasterStack(filename_dem = paste0("data-raw/raster.dem_",
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
            plot(r, col = dem_colfunc((24 - 9)*2), legend.width = 1,
                 horizontal = TRUE, bty = "n",
                 legend.args = list(text = "elevation (m)"))
            # points(spdf.gsd_L, pch = 21, bg = "white")
            # text(spdf.gsd_L[1,], pos = 3, labels = spdf.gsd_L$gauging_station[1])
            # text(spdf.gsd_L[2,], pos = 1, labels = spdf.gsd_L$gauging_station[2])
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
                flood_extent <- flood3(x, as.Date(a_date))
                flood_extent[flood_extent == 0] <- NA
                
                # plotting with raster functions
                png(filename = f_out, width = 960, height = 640, units = "px")
                plot(r, col = dem_colfunc((24 - 9)*2),
                     legend.width = 1, horizontal = TRUE, bty = "n",
                     legend.args = list(text = "elevation (m)"))
                plot(flood_extent, col = "blue", add = TRUE, legend = FALSE)
                # points(spdf.gsd_L, pch = 21, bg = "white")
                # text(spdf.gsd_L[1,], pos = 3, labels = spdf.gsd_L$gauging_station[1])
                # text(spdf.gsd_L[2,], pos = 1, labels = spdf.gsd_L$gauging_station[2])
                dev.off()
                
            }
        }
    }
}



#
q("no")
