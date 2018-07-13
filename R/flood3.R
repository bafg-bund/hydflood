#' @name flood3
#' @rdname flood3
#' 
#' @title Function to compute flood extent or flood duration along German 
#'   federal waterways Elbe and Rhine
#' 
#' @description 
#'
#' @details
#' 
#' @param x
#' @param start
#' @param end
#' 
#' @return
#' 
#' @seealso \code{\link{plotShiny}}
#'
#' @references 
#' 
#' @export
#' 
flood3 <- function(x, start, end) {
    
    # make parent environment accessible through the local environment
    e <- environment()
    p_env <- parent.env(e)
    
    ## vector and function to catch error messages
    errors <- character()
    l <- function(errors) {as.character(length(errors) + 1)}
    
    ## x
    if (missing(x)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The 'x' ",
                                   "argument has to be supplied."))
    } else {
        # class
        if (class(x) != "RasterStack") {
            errors <- c(errors, paste0("Error ", l(errors), ": The 'x' ",
                                       "argument has to be supplied."))
        }
        # names
        if (!(all(names(x) == c("csa", "dem")))) {
            errors <- c(errors, paste0("Error ", l(errors), ": names(x) ",
                                       "must be c('csa', 'dem')."))
        }
        
        # crs
        crs_string <- crs(x, asText = TRUE)
        if (!(grepl("+proj=utm", crs_string))) {
            errors <- c(errors, paste0("Error ", l(errors), ": The projection",
                                       " of crs(x) must be 'utm'."))
        }
        if ((!(grepl("+zone=32", crs_string))) & 
            (!(grepl("+zone=33", crs_string)))) {
            errors <- c(errors, paste0("Error ", l(errors), ": The zone",
                                       " of crs(x) must be either 32 or 33."))
        }
        if (grepl("+zone=32", crs_string)) {
            zone <- "32"
            river <- "Rhein"
        }
        if (grepl("+zone=33", crs_string)) {
            zone <- "33"
            river <- "Elbe"
        }
        if (!(grepl("+ellps=GRS80", crs_string))) {
            errors <- c(errors, paste0("Error ", l(errors), ": The elipsoid",
                                       " of crs(x) must be 'GRS80'."))
        }
        if (!(grepl("+units=m", crs_string))) {
            errors <- c(errors, paste0("Error ", l(errors), ": The unit",
                                       " of crs(x) must be 'm'."))
        }
        if (!(grepl("+no_defs", crs_string))) {
            errors <- c(errors, paste0("Error ", l(errors), ": +no_defs",
                                       " must be part of crs(x)."))
        }
        
        # check position
        if ('river' %in% ls()){
            # access the spdf.active_floodplain_* data
            active_floodplain <- paste0("spdf.active_floodplain_", river)
            if (exists(active_floodplain, where = p_env)){
                get(active_floodplain, envir = p_env)
            } else {
                utils::data(active_floodplain)
            }
            
            
        }
    }
    
    
    
    
    return(fd)
}


#' @examples
require(hyd1d)
require(sp)
require(rgdal)
require(raster)
#require(hydflood3)

csa <- raster(system.file("data-raw/raster.csa.tif"))
dem <- raster(system.file("data-raw/raster.dem.tif"))

csa <- raster("data-raw/raster.csa.tif")
dem <- raster("data-raw/raster.dem.tif")

x <- stack(csa, dem)
names(x) <- c("csa", "dem")
