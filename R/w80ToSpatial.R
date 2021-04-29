#' @name w80ToSpatialLinesDataFrame
#' @rdname w80ToSpatialLinesDataFrame
#' 
#' @title Function to convert w80-files to \code{SpatialLinesDataFrame})
#' 
#' @description This function converts w80-files, an asci-format with 80
#'   characters per line for spatial point data used by the German Waterway and
#'   Navigation Authorities (WSV). Every single row codes for one point:
#'   
#'   \code{| 1 |2 | 3     |4|            5              |       6       |     7   | 8 |}
#'   \code{W0701 55 2279152 01453491549357491793252804043 6424102804045  935  Bu.21 01}
#'   \code{W0701495 2279152 02453491287057491789982804043 6391902804045  935  Bu.21 01}
#'   
#'   Within each row very specific sections code for specific attributes:
#'   
#'   | section | column(s) | attribute | colum name result |
#'   | 1 | 1 | state id, here W=WSV | sid |
#'   | 1 | 2-5 | Federal Waterway ID | fwid |
#'   | 2 | 6-8 | WSV point type | wsvpt |
#'   | 3 | 9 | blank | - |
#'   | 3 | 10-15 | river station (km) | station |
#'   | 4 | 16 | bank: 1 left, 2 right | bank |
#'   | 4 | 17-20 | continuous id | id |
#'   | 5 | 21-30 | easting in GK-coordinates | x |
#'   | 5 | 31-40 | northing in GK-coordinates | y |
#'   | 6 | 41-46 | datum of measurement | date_coor |
#'   | 6 | 47 | accuracy | acc_coor |
#'   | 6 | 48-54 | elevation | z |
#'   | 6 | 55-60 | date of the elevation measurement | date_z |
#'   | 6 | 61 | accuracy of the elevation measurement | acc_z |
#'   | 6 | 62-64 | type of measurement | tom |
#'   | 7 | 65-84 | comment | comment |
#'   | 8 | 85-86 | point status | status |
#'   
#'   In a second step these points are aggregated to a
#'   \code{SpatialLinesDataFrame} using the grouping column \code{group}.
#' 
#' @param filename argument of length 1 and type \code{character} specifying
#'   an existing w80-file.
#' @param crs argument of type \code{CRS}.
#' @param group argument of type \code{character} specifying a grouping column.
#' 
#' @return \code{SpatialLinesDataFrame}.
#' 
#' @examples \dontrun{
#'   library(hydflood)
#'   c <- crs("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")
#'   splf <- w80ToSpatialLinesDataFrame("data-raw/test.w80", c)
#' }
#' 
#' @export
#' 
w80ToSpatialLinesDataFrame <- function(filename, crs,
                                       id = c("sid", "fwid", "wsvpt",
                                              "station", "bank", "id", "x",
                                              "y", "date_coor", "acc_coor",
                                              "z", "date_z", "acc_z", "tom",
                                              "comment", "status", "lat",
                                              "lon", "station_int",
                                              "station_c")) {
    
    # check input
    stopifnot(is.character(filename),length(filename) == 1,
              file.exists(filename))
    stopifnot(class(crs) == "CRS")
    
    # read all lines
    lines <- readLines(con <- file(filename, encoding="UTF-8"), warn = FALSE)
    close(con)
    l <- length(lines)
    
    # create an empty df
    df <- data.frame(sid = as.character(rep(NA, l)),
                     fwid = as.integer(rep(NA, l)),
                     wsvpt = as.integer(rep(NA, l)),
                     station = as.numeric(rep(NA, l)),
                     bank = as.character(rep(NA, l)),
                     id = as.integer(rep(NA, l)),
                     x = as.numeric(rep(NA, l)),
                     y = as.numeric(rep(NA, l)),
                     date_coor = as.Date(rep(NA, l)),
                     acc_coor = as.integer(rep(NA, l)),
                     z = as.numeric(rep(NA, l)),
                     date_z = as.Date(rep(NA, l)),
                     acc_z = as.integer(rep(NA, l)),
                     tom = as.character(rep(NA, l)),
                     comment = as.character(rep(NA, l)),
                     status = as.character(rep(NA, l)),
                     lat = as.numeric(rep(NA, l)),
                     lon = as.numeric(rep(NA, l)),
                     row.names = 1:l,
                     stringsAsFactors = FALSE)
    bank <- c("left", "right")
    
    # read data to df
    df$sid <- substring(lines, 1, 1)
    df$sid[which(df$sid == " ")] <- NA
    df$fwid <- as.integer(substring(lines, 2, 5))
    df$fwid[which(df$fwid == "    ")] <- NA
    df$wsvpt <- as.integer(substring(lines, 6, 8))
    df$station <- as.numeric(substring(lines, 10, 15)) / 1000
    df$bank <- bank[as.integer(substring(lines, 16, 16))]
    df$id <- as.integer(substring(lines, 17, 20))
    df$x <- as.numeric(substring(lines, 21, 30)) / 1000
    df$y <- as.numeric(substring(lines, 31, 40)) / 1000
    df$date_coor <- as.Date(strptime(substring(lines, 41, 46), format="%d%m%y"))
    df$acc_coor <- substring(lines, 47, 47)
    df$acc_coor[which(df$acc_coor == " ")] <- NA
    df$z <- as.numeric(substring(lines, 48, 54))/10000
    df$date_z <- as.Date(strptime(substring(lines, 55, 60), format="%d%m%y"))
    df$acc_z <- substring(lines, 61, 61)
    df$acc_z[which(df$acc_z == " ")] <- NA
    df$tom <- substring(lines, 62, 64)
    df$tom[which(df$tom == "   ")] <- NA
    df$comment <- substring(lines, 65, 78)
    df$comment[which(df$comment == "              ")] <- NA
    df$status <- substring(lines, 79, 80)
    df$station_int <- as.integer(df$station * 1000)
    df$station_c <- as.character(df$station_int)
    sp::coordinates(df) <- ~x+y
    
    ## list of Lines per id, each with one Line in a list
    ids <- unique(df@data[, id])
    Srl <- vector("list", length(ids))
    names(Srl) <- paste0("i", ids)
    j <- 1
    for (i in ids) {
        d <- df[which(df@data[, id] == i), ]
        Srl[[j]] <- Lines(list(Line(coordinates(d))), ID = paste0("i", i))
        j <- j + 1
    }
    
    sl <- SpatialLines(Srl, proj4string = crs)
    sldf <- SpatialLinesDataFrame(sl,
                                  data.frame(id == ids,
                                             station = unique(df$station),
                                             station_int = unique(df$station_int),
                                             row.names = paste0("i", ids)))
    
    return(sldf)
}


#' @name w80ToSpatialPointsDataFrame
#' @rdname w80ToSpatialPointsDataFrame
#' 
#' @title Function to convert w80-files to \code{SpatialPointsDataFrame})
#' 
#' @description This function converts w80-files, an asci-format with 80
#'   characters per line for spatial point data used by the German Waterway and
#'   Navigation Authorities (WSV). Every single row codes for one point:
#'   
#'   \code{| 1 |2 | 3     |4|            5              |       6       |     7   | 8 |}
#'   \code{W0701 55 2279152 01453491549357491793252804043 6424102804045  935  Bu.21 01}
#'   \code{W0701495 2279152 02453491287057491789982804043 6391902804045  935  Bu.21 01}
#'   
#'   Within each row very specific sections code for specific attributes:
#'   
#'   | section | column(s) | attribute | colum name result |
#'   | 1 | 1 | state id, here W=WSV | sid |
#'   | 1 | 2-5 | Federal Waterway ID | fwid |
#'   | 2 | 6-8 | WSV point type | wsvpt |
#'   | 3 | 9 | blank | - |
#'   | 3 | 10-15 | river station (km) | station |
#'   | 4 | 16 | bank: 1 left, 2 right | bank |
#'   | 4 | 17-20 | continuous id | id |
#'   | 5 | 21-30 | easting in GK-coordinates | x |
#'   | 5 | 31-40 | northing in GK-coordinates | y |
#'   | 6 | 41-46 | datum of measurement | date_coor |
#'   | 6 | 47 | accuracy | acc_coor |
#'   | 6 | 48-54 | elevation | z |
#'   | 6 | 55-60 | date of the elevation measurement | date_z |
#'   | 6 | 61 | accuracy of the elevation measurement | acc_z |
#'   | 6 | 62-64 | type of measurement | tom |
#'   | 7 | 65-84 | comment | comment |
#'   | 8 | 85-86 | point status | status |
#' 
#' @param filename argument of length 1 and type \code{"character"} specifying
#'   an existing w80-file.
#' @param crs argument of type \code{CRS}
#' 
#' @return \code{SpatialPointsDataFrame}.
#' 
#' @examples \dontrun{
#'   library(hydflood)
#'   c <- crs("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")
#'   spdf <- w80ToSpatialPointsDataFrame("data-raw/test.w80", c)
#' }
#' 
#' @export
#' 
w80ToSpatialPointsDataFrame <- function(filename, crs) {
    
    # check input
    stopifnot(is.character(filename),length(filename) == 1,
              file.exists(filename))
    stopifnot(class(crs) == "CRS")
    
    # read all lines
    lines <- readLines(con <- file(filename, encoding="UTF-8"), warn = FALSE)
    close(con)
    l <- length(lines)
    
    # create an empty df
    df <- data.frame(sid = as.character(rep(NA, l)),
                     fwid = as.integer(rep(NA, l)),
                     wsvpt = as.integer(rep(NA, l)),
                     station = as.numeric(rep(NA, l)),
                     bank = as.character(rep(NA, l)),
                     id = as.integer(rep(NA, l)),
                     x = as.numeric(rep(NA, l)),
                     y = as.numeric(rep(NA, l)),
                     date_coor = as.Date(rep(NA, l)),
                     acc_coor = as.integer(rep(NA, l)),
                     z = as.numeric(rep(NA, l)),
                     date_z = as.Date(rep(NA, l)),
                     acc_z = as.integer(rep(NA, l)),
                     tom = as.character(rep(NA, l)),
                     comment = as.character(rep(NA, l)),
                     status = as.character(rep(NA, l)),
                     lat = as.numeric(rep(NA, l)),
                     lon = as.numeric(rep(NA, l)),
                     row.names = 1:l,
                     stringsAsFactors = FALSE)
    bank <- c("left", "right")
    
    # read data to df
    df$sid <- substring(lines, 1, 1)
    df$sid[which(df$sid == " ")] <- NA
    df$fwid <- as.integer(substring(lines, 2, 5))
    df$fwid[which(df$fwid == "    ")] <- NA
    df$wsvpt <- as.integer(substring(lines, 6, 8))
    df$station <- as.numeric(substring(lines, 10, 15)) / 1000
    df$bank <- bank[as.integer(substring(lines, 16, 16))]
    df$id <- as.integer(substring(lines, 17, 20))
    df$x <- as.numeric(substring(lines, 21, 30)) / 1000
    df$y <- as.numeric(substring(lines, 31, 40)) / 1000
    df$date_coor <- as.Date(strptime(substring(lines, 41, 46), format="%d%m%y"))
    df$acc_coor <- substring(lines, 47, 47)
    df$acc_coor[which(df$acc_coor == " ")] <- NA
    df$z <- as.numeric(substring(lines, 48, 54))/10000
    df$date_z <- as.Date(strptime(substring(lines, 55, 60), format="%d%m%y"))
    df$acc_z <- substring(lines, 61, 61)
    df$acc_z[which(df$acc_z == " ")] <- NA
    df$tom <- substring(lines, 62, 64)
    df$tom[which(df$tom == "   ")] <- NA
    df$comment <- substring(lines, 65, 78)
    df$comment[which(df$comment == "              ")] <- NA
    df$status <- substring(lines, 79, 80)
    
    # make spatial
    spdf <- SpatialPointsDataFrame(df[,c("x", "y")],
                                   df, proj4string = crs)
    
    # add columns lat and lon
    spdf$lat <- coordinates(sp::spTransform(spdf,
                                            CRS(SRS_string = "OGC:CRS84")))[,2]
    spdf$lon <- coordinates(sp::spTransform(spdf,
                                            CRS(SRS_string = "OGC:CRS84")))[,1]
    
    return(spdf)
}

