
library(stringr)
library(sf)
library(terra)
library(tools)
source("R/hydflood-internal.R")

utm33 <- sf::st_crs(25833)
utm32 <- sf::st_crs(25832)

el <- list.files("~/freigaben/U/U3/Auengruppe_INFORM/EL_000_586_UFD/data/tiff/flood3",
                 "*.tif")
elp <- list.files("~/freigaben/U/U3/Auengruppe_INFORM/EL_000_586_UFD/data/tiff/flood3",
                  "*.tif", full.names = TRUE)
id.exc <- endsWith(el, "1990_2020.tif") | endsWith(el, "1990_2021.tif") | endsWith(el, ".tif.aux.xml")
el <- el[!id.exc]
elp <- elp[!id.exc]

rh <- list.files("~/freigaben/U/U3/Auengruppe_INFORM/RH_336_867_UFD/data/tiff/flood3",
                 "*.tif")
rhp <- list.files("~/freigaben/U/U3/Auengruppe_INFORM/RH_336_867_UFD/data/tiff/flood3",
                  "*.tif", full.names = TRUE)
id.exc <- endsWith(rh, "1990_2020.tif") | endsWith(rh, "1990_2021.tif")
rh <- rh[!id.exc]
rhp <- rhp[!id.exc]

df <- data.frame(fn = c(el, rh), path = c(elp, rhp),
                 epsg = c(rep(25833, length(el)), rep(25832, length(rh))))
df$filename <- gsub(".tif", "", df$fn)
df$fileformat <- "tif"
df$dataset_type <- "annual"
df$dataset_type[grepl("mean", df$filename)] <- "aggregated"
df$dataset_type[grepl("sd", df$filename)] <- "aggregated"
df$dataset_type[grepl("potential_natural_vegetation", df$filename)] <- "pnv"
id.agg <- df$dataset_type == "aggregated"
id.pnv <- df$dataset_type == "pnv"

df$date_start <- paste0(substring(df$filename, nchar(df$filename) - 3,
                                  nchar(df$filename)),
                        "-01-01")
df$date_end <- paste0(substring(df$filename, nchar(df$filename) - 3,
                                nchar(df$filename)),
                      "-12-31")
df$date_start[id.agg] <- paste0(substring(df$filename[id.agg],
                                          nchar(df$filename[id.agg]) - 8,
                                          nchar(df$filename[id.agg]) - 5),
                                 "-01-01")
df$date_start[id.pnv] <- paste0(substring(df$filename[id.pnv],
                                          nchar(df$filename[id.pnv]) - 37,
                                          nchar(df$filename[id.pnv]) - 34),
                                "-01-01")
df$date_end[id.pnv] <- paste0(substring(df$filename[id.pnv],
                                        nchar(df$filename[id.pnv]) - 32,
                                        nchar(df$filename[id.pnv]) - 29),
                                "-12-31")

df$url <- ""
df$filesize <- NA_real_
df$tilename <- NA_character_
df$utm_east_min <- NA_real_
df$utm_east_max <- NA_real_
df$utm_north_min <- NA_real_
df$utm_north_max <- NA_real_
df$longitude_min <- NA_real_
df$longitude_max <- NA_real_
df$latitude_min <- NA_real_
df$latitude_max <- NA_real_
df$md5sum <- md5sum(df$path)

tilename <- "."

for (i in 1:nrow(df)) {
    print(i)
    if (!startsWith(df$filename[i], tilename)) {
        
        tilename <- paste0(unlist(strsplit(df$filename[i], "_"))[1:2],
                           collapse = "_")
        
        if (startsWith(tilename, "e")) {
            crs <- utm33
        } else {
            crs <- utm32
        }
        r <- rast(df$path[i])
        
        utm_east_min <- ext(r)$xmin
        utm_east_max <- ext(r)$xmax
        utm_north_min <- ext(r)$ymin
        utm_north_max <- ext(r)$ymax
        
        p <- extent2polygon(ext(r), crs)
        p_wgs <- sf::st_transform(p, sf::st_crs(4326))
        bb <- sf::st_bbox(p_wgs)
        longitude_min <- bb$xmin
        longitude_max <- bb$xmax
        latitude_min <- bb$ymin
        latitude_max <- bb$ymax
    }
    
    df$tilename[i] <- tilename
    df$filesize[i] <- file.size(df$path[i])/1000
    df$utm_east_min[i] <- utm_east_min
    df$utm_east_max[i] <- utm_east_max
    df$utm_north_min[i] <- utm_north_min
    df$utm_north_max[i] <- utm_north_max
    df$longitude_min[i] <- longitude_min
    df$longitude_max[i] <- longitude_max
    df$latitude_min[i] <- latitude_min
    df$latitude_max[i] <- latitude_max
}

df.export <- df[, c("date_start", "date_end", "filename", "fileformat",
                    "filesize", "url", "utm_east_min", "utm_east_max",
                    "utm_north_min", "utm_north_max", "longitude_min",
                    "longitude_max", "latitude_min", "latitude_max",
                    "tilename", "dataset_type", "epsg", "md5sum")]

write.table(df.export, file = "vignettes/pangaea.tab", sep = "\t", dec = ".",
            row.names = FALSE, quote = FALSE)

write.table(df, file = "vignettes/pangaea_all.tab", sep = "\t", dec = ".",
            row.names = FALSE, quote = FALSE)

q("no")
