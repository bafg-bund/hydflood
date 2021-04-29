library(hydflood)
library(leaflet)

filename <- "/home/WeberA/freigaben/Projekte/FLYS_3/DaMiF-FLYS3-Projekt/03_Daten/gewaesser_work/Oder/geodaesie/querprofile/QP-Daten/WSV-Daten/Oder.xyz"
wgs84 <- CRS(SRS_string = "OGC:CRS84")
c <- CRS(SRS_string = "EPSG:2399")

spdf <- w80ToSpatialPointsDataFrame(filename = filename,
                                    crs = c)

sldf <- w80ToSpatialLinesDataFrame(filename = filename,
                                   crs = c, id = "station_int")
sldf.w <- spTransform(sldf, wgs84)


leaflet() %>% addTiles() %>% addPolylines(data = sldf.w)
