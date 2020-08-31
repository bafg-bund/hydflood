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
#' @import stats
#' @importFrom sp coordinates
#' @importFrom sp CRS
#' @importFrom sp Polygon
#' @importFrom sp Polygons
#' @importFrom sp SpatialPolygons
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom raster crs
#' @importFrom raster compareCRS
#' @importFrom raster raster
#' @importFrom raster unique
#' @importFrom raster canProcessInMemory
#' @importFrom raster trim
#' @importFrom raster writeStart
#' @importFrom raster blockSize
#' @importFrom raster pbCreate
#' @importFrom raster getValues
#' @importFrom raster writeValues
#' @importFrom raster pbStep
#' @importFrom raster writeStop
#' @importFrom raster setValues
#' @importFrom raster pbClose
#' @importFrom raster extent
#' @importFrom raster nlayers
#' @importFrom raster res
#' @importFrom raster dataType
#' @importFrom raster crop
#' @importFrom raster intersect
#' @importFrom raster rasterize
#' @importFrom raster merge
#' @importFrom raster extend
#' @importFrom raster writeRaster
#' @importFrom rgeos gContains
#' 
NULL
