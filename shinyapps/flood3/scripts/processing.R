################################################################################
# processing.R
#
# author: arnd.weber@bafg.de
# date:   12.09.2018
#
# purpose: 
#   - compute flood3 based on the input parameters stored in args
#   - send an email with a download link of the stored product
#
################################################################################

# standard library path for the packages
R_version <- paste(sep = ".", R.Version()$major, R.Version()$minor)
lib <- paste0("~/R/", R_version, "/")

# load the necessary packages
library(sp, lib.loc = lib)
library(raster, lib.loc = lib)
library(rgdal, lib.loc = lib)
library(rgeos, lib.loc = lib)
library(hyd1d, lib.loc = lib)
library(hydflood3, lib.loc = lib)
library(inspiRe, lib.loc = lib)

# add functions and definitions
# function to convert an extent to a polygon
extent2polygon <- function(x, crs) {
    df.corners <- data.frame(x = c(x@xmin, x@xmax, x@xmax, x@xmin, x@xmin),
                             y = c(x@ymin, x@ymin, x@ymax, x@ymax, x@ymin))
    ma.corners <- as.matrix(df.corners)
    p.polygon <- sp::Polygon(ma.corners, FALSE)
    p.polygons <- sp::Polygons(list(p.polygon), ID = "1")
    sp.polygon <- sp::SpatialPolygons(list(p.polygons), 
                                      proj4string = crs)
    return(sp.polygon)
}

# define wgs 84
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#####
# capture args, load input data and prepare result directories
path <- commandArgs(trailingOnly = TRUE)[[1]]
#path <- "in_process/7f199JoieFF0VFJQrKZk.RData"
#path <- "in_process/hy86051lzdb61phjalce.RData"
id <- gsub(".RData", "", unlist(strsplit(path, "/"))[2])

# load data
load(path)

#####
# prepare the computation inputs
###
# x
ext <- extent(l.res$extent)
crs <- crs(l.res$crs)
x <- hydRasterStack(ext = ext, crs = crs)

# seq
seq <- seq.Date(from = as.Date(l.res$seq_from_to[1]), 
                to = as.Date(l.res$seq_from_to[2]),
                by = "days")

# create a directory for results
dir.create(paste0("processed/", id), FALSE, TRUE)

# filename 
filename <- paste0("processed/", id, "/", l.res$river, "_", 
                   paste0(l.res$extent, collapse = "-"), "_",
                   paste0(l.res$seq_from_to, collapse = "-"), ".tif")
filename_xml <- paste0(filename, ".xml")
filename_msg <- gsub(".tif", ".msg", filename)
filename_zip <- paste0("www/downloads/", id, ".zip")
filename_zip_msg <- paste0("downloads/", id, ".zip")

#####
# compute
f <- flood3(x = x, seq = seq, filename = filename)

#####
# create INSPIRE, ISO 19139, GGinA-conform metadata
# create an object of "inspireSettings"
m <- new("inspireSettings")

# add title and abstract
m@ResourceTitle <- "Überflutungsdauer nach hydflood3::flood3()"
m@ResourceAbstract <- paste0("Dieser Rasterdatensatz der Überflutungsdau",
                             "er eines Teiles der aktiven ", l.res$river, "-aue ",
                             "ist mittels des R-Paketes hydflood3 ber",
                             "echnet. Der Datensatz hat eine räumliche A",
                             "uflösung von 1 Meter und weißt Werte zwisc",
                             "hen 0 und maximal ", length(seq), " Tagen ",
                             "auf.")

# link to online ressources
m <- setResourceLocator(m, paste0("http://hpc-service.bafg.de/~WeberA/hydflood",
                                  "3/index.html"))
m <- addResourceLocator(m, paste0("http://hpc-service.bafg.de/shiny/WeberA/07-",
                                  "flood3/"))

# add the authors information
m <- setAuthor(m, "Dr. Arnd Weber", "++49 (0)261/1306-5462", 
               "arnd.weber@bafg.de","Ansprechpartner")

m@ResourceCreationDate   <- as.character(Sys.Date())
m@ResourceCreationDateType<-"Date of last revision"

m@Version                <- paste0("hydflood3, version ", 
                                   packageVersion("hydflood3"))
m@TemporalExtent         <- c(l.res$seq_from_to[1], l.res$seq_from_to[2])

# compute the bounding box
sp.ext <- extent2polygon(x = ext, crs = crs)
sp.ext_wgs84 <- spTransform(sp.ext, CRSobj = wgs84)
e.ext_wgs84 <- extent(sp.ext_wgs84)
bb.ext_wgs84 <- c(e.ext_wgs84@xmin, e.ext_wgs84@xmax, 
                  e.ext_wgs84@ymin, e.ext_wgs84@ymax) 
m@BoundingBox            <- bb.ext_wgs84

m@DistributionFormatName <- "GeoTIFF"
m@DistributionFormatVersion <- "1.8.1"
m@UseLimitations         <- paste0("Die Daten (GeoTIFF-Format) sind mit Standa",
                                   "rd-GIS-Software les- und bearbeitbar.")
m@ConditionOfUse         <- paste0("Die Daten werden dem Nutzer ohne jede Gewä",
                                   "hrleistung überlassen. Der Nutzer ist sich",
                                   " bewusst, dass die Daten mit Unsicherheite",
                                   "n behaftet sind, da sie auf der Basis morp",
                                   "hologischer und hydrologischer Modelle gen",
                                   "eriert wurden und Gegenstand laufender For",
                                   "schung- und Entwicklungstätigkeiten sind. ",
                                   "Für mögliche Schäden, die aus Ungenauigkei",
                                   "ten resultieren, übernimmt die BfG keine H",
                                   "aftung. Der Nutzer verpflichtet sich, in V",
                                   "eröffentlichungen, die auf der Grundlage d",
                                   "er bereitgestellten Daten entstanden sind,",
                                   " die BfG als Datenquelle zu nennen und das",
                                   " Berechnungsrelevante R-Paket hydflood3 zu",
                                   "zitieren. Die unentgeltliche Übermittelung",
                                   " eines entsprechendes Belegexemplars ist g",
                                   "ewünscht.")
m@IdentifierCode         <- paste0("Shiny-URL")
m@IdentifierCodespace    <- "http://doi.bafg.de"

# Thema, Schlagworte
m@TopicCategory          <- factor(c("Inland Waters", "Environment"),
                                   levels = levels(m@TopicCategory))
m                        <- setKeyword(m, Name = "inspireidentifiziert")
m                        <- addKeyword(m, "Gewässernetz", 
                                       Dictionary = "INSPIRE-Annex-Thema", 
                                       "2014-04-17")

m@CharacterSetCode <- "utf8"
m@ResourceType <- "dataset"
m@MetadataLanguage <- "ger"
m@ResourceLanguage <- "ger"

# save the xml file
xml <- xmlInspireCreate(m)
saveXML(doc = xml, file = filename_xml, encoding = "UTF-8")

#####
# assemble the email message
# header
write(paste0("Sehr geehrte Nutzerin, sehr geehrter Nutzer,\n\nsoeben wurde ihr",
             "e Berechnung der Überflutungsdauer abgeschlossen. Unter \nfolgen",
             "dem Link können Sie Ihr Berechnungsprodukt bis zum ", 
             strftime(Sys.Date() + 7, "%d.%m.%Y"), " herunterladen: \n"), 
      file = filename_msg)

# link
write(paste0("http://hpc-service.bafg.de/shiny/WeberA/07-flood3/", 
             filename_zip_msg, "\n"), file = filename_msg, append = TRUE)

# footer
write(paste0("Mit freundlichen Grüßen\nIm Auftrag\nIhre BfG\n\n"), 
      file = filename_msg, append = TRUE)

#####
# zip the tif, xml and msg file for downloading
wd_old <- getwd()
setwd(paste0(wd_old, "/processed/", id))
system(paste0("mv ", wd_old, "/in_process/", id, ".RData ."))
zip(zipfile = paste0(id, ".zip"), files = list.files())
setwd(wd_old)
system(paste0("mv ", getwd(), "/processed/", id, "/", id, ".zip ", getwd(), 
              "/", filename_zip))

#####
# send an email with the download link
system(paste0("mail -s '[shiny-flood3]: Berechnung abgeschlossen' ", l.res$email, " < ", 
              filename_msg))

#####
# clean up 

#####
# quit R
q("no")
