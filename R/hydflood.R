#' @name hydflood
#' @docType package
#' 
#' @title hydflood: A package to compute flood extent and duration along 
#'    German federal waterways Elbe and Rhine
#' 
#' @description To be done...
#' 
#' @import hyd1d
#' @importFrom hoardr hoard
#' @importFrom Rdpack reprompt
#' @importFrom stats na.omit
#' @importFrom raster canProcessInMemory
#' @importFrom raster blockSize
#' @importFrom raster pbCreate
#' @importFrom raster getValues
#' @importFrom raster writeValues
#' @importFrom raster pbStep
#' @importFrom raster setValues
#' @importFrom raster pbClose
#' @importFrom terra crs
#' @importFrom terra rast
#' @importFrom terra unique
#' @importFrom terra trim
#' @importFrom terra writeStart
#' @importFrom terra writeStop
#' @importFrom terra ext
#' @importFrom terra res
#' @importFrom terra crop
#' @importFrom terra intersect
#' @importFrom terra rasterize
#' @importFrom terra merge
#' @importFrom terra extend
#' @importFrom terra vect
#' @importFrom terra writeRaster
#' @importFrom terra xmin
#' @importFrom terra xmax
#' @importFrom terra ymin
#' @importFrom terra ymax
#' @importFrom sf st_sf
#' @importFrom sf st_sfc
#' @importFrom sf st_crs
#' @importFrom sf st_as_sfc
#' @importFrom sf st_bbox
#' @importFrom sf st_as_sf
#' @importFrom sf st_coordinates
#' @importFrom sf st_transform
#' @importFrom sf st_join
#' 
NULL
