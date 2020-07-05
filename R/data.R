#' @name spdf.active_floodplain_elbe
#' @rdname spdf.active_floodplain_elbe
#' 
#' @title Active floodplain along River Elbe
#' 
#' @description This dataset contains a polygon of the active floodplain along
#'    the German interior parts of River Elbe from the Czech border to the
#'    weir in Geesthacht in the coordinate reference system 
#'    \href{http://spatialreference.org/ref/epsg/etrs89-utm-zone-33n/}{ETRS 1989 UTM 33N}.
#' 
#'   Originally, this polygon was produced for the floodplain status report 
#'   (Auenzustandsbericht; Brunotte et al. (2009), Bundesamt für Naturschutz 
#'   (2009)) at a scale of 1:25000. For hydflood it was updated with recent 
#'   flood protection measures and manually improved with recent digital 
#'   elevation models and aerial images at a scale < 1:10000.
#' 
#' @format A \code{SpatialPolygonsDataFrame} containing 1 polygon 
#' 
#' @references 
#'   \insertRef{brunotte_flussauen_2009}{hydflood}
#'   
#'   \insertRef{brunotte_flussauen_data_2009}{hydflood}
#'   
#'   \insertRef{bfn_auenzustandsbericht_2009}{hydflood}
#' 
"spdf.active_floodplain_elbe"

#' @name spdf.active_floodplain_rhein
#' @rdname spdf.active_floodplain_rhein
#' 
#' @title Active floodplain along River Rhine
#' 
#' @description This dataset contains a polygon of the active floodplain along
#'   the German, freeflowing parts of River Rhine from the weir Iffezheim and 
#'   the Dutch border in the coordinate reference system 
#'   \href{http://spatialreference.org/ref/epsg/etrs89-utm-zone-32n/}{ETRS 1989 UTM 32N}.
#' 
#'   Originally, this polygon was produced for the floodplain status report 
#'   (Auenzustandsbericht; Brunotte et al. (2009), Bundesamt für Naturschutz 
#'   (2009)) at a scale of 1:25000. For hydflood it was updated with recent 
#'   flood protection measures and manually improved with recent digital 
#'   elevation models and aerial images at a scale < 1:10000.
#' 
#' @format A \code{SpatialPolygonsDataFrame} containing 1 polygon 
#' 
#' @references 
#'   \insertRef{brunotte_flussauen_2009}{hydflood}
#'   
#'   \insertRef{brunotte_flussauen_data_2009}{hydflood}
#'   
#'   \insertRef{bfn_auenzustandsbericht_2009}{hydflood}
#' 
"spdf.active_floodplain_rhein"

#' @name spdf.tiles_elbe
#' @rdname spdf.tiles_elbe
#' 
#' @title Tiling along the active floodplain of River Elbe
#' 
#' @description This dataset contains 49 rectangular polygons / tiles along the
#'    active floodplain along the German interior parts of River Elbe from the
#'    Czech border to the weir in Geesthacht in the coordinate reference system
#'    \href{http://spatialreference.org/ref/epsg/etrs89-utm-zone-33n/}{ETRS 1989 UTM 33N}.
#'   
#'   The tiles represent the original tiling of the internally used digital
#'   elevation model (Weber 2020).
#' 
#' @format A \code{SpatialPolygonsDataFrame} containing 49 polygons with 11 attributes:
#' \describe{
#'   \item{id}{of the tile (type \code{integer}).} 
#'   \item{name}{of the tile (type \code{character}).} 
#'   \item{xmin}{of the tile extent (type \code{numeric}). Minimum of UTM Easting (m).}
#'   \item{xmax}{of the tile extent (type \code{numeric}). Maximum of UTM Easting (m).}
#'   \item{ymin}{of the tile extent (type \code{numeric}). Minimum of UTM Northing (m).}
#'   \item{ymax}{of the tile extent (type \code{numeric}). Maximum of UTM Northing (m).}
#'   \item{lon_min}{of the tile extent (type \code{numeric}). Minimum of Longitude (decimal °).}
#'   \item{lon_max}{of the tile extent (type \code{numeric}). Maximum of Longitude (decimal °).}
#'   \item{lat_min}{of the tile extent (type \code{numeric}). Minimum of Latitude (decimal °).}
#'   \item{lat_max}{of the tile extent (type \code{numeric}). Maximum of Latitude (decimal °).}
#'   \item{url}{of the tile (type \code{character}).}
#' }
#' 
#' @references 
#'   \insertRef{weber_dgms_2020}{hydflood}
#'   
#'   \insertRef{weber_dgm_elbe_2020}{hydflood}
#' 
"spdf.tiles_elbe"

#' @name spdf.tiles_rhein
#' @rdname spdf.tiles_rhein
#' 
#' @title Tiling along the active floodplain of River Rhine
#' 
#' @description This dataset contains 40 rectangular polygons / tiles along the
#'    active floodplain along the German, freeflowing parts of River Rhine from
#'    the weir Iffezheim to the Dutch border near Kleve in the coordinate
#'    reference system
#'    \href{http://spatialreference.org/ref/epsg/etrs89-utm-zone-32n/}{ETRS 1989 UTM 32N}.
#'   
#'   The tiles represent the original tiling of the internally used digital
#'   elevation model (Weber 2020).
#' 
#' @format A \code{SpatialPolygonsDataFrame} containing 40 polygons with 11 attributes:
#' \describe{
#'   \item{id}{of the tile (type \code{integer}).} 
#'   \item{name}{of the tile (type \code{character}).} 
#'   \item{xmin}{of the tile extent (type \code{numeric}). Minimum of UTM Easting (m).}
#'   \item{xmax}{of the tile extent (type \code{numeric}). Maximum of UTM Easting (m).}
#'   \item{ymin}{of the tile extent (type \code{numeric}). Minimum of UTM Northing (m).}
#'   \item{ymax}{of the tile extent (type \code{numeric}). Maximum of UTM Northing (m).}
#'   \item{lon_min}{of the tile extent (type \code{numeric}). Minimum of Longitude (decimal °).}
#'   \item{lon_max}{of the tile extent (type \code{numeric}). Maximum of Longitude (decimal °).}
#'   \item{lat_min}{of the tile extent (type \code{numeric}). Minimum of Latitude (decimal °).}
#'   \item{lat_max}{of the tile extent (type \code{numeric}). Maximum of Latitude (decimal °).}
#'   \item{url}{of the tile (type \code{character}).}
#' }
#'
#' @references
#'   \insertRef{weber_dgms_2020}{hydflood}
#'   
#'   \insertRef{weber_dgm_rhine_2020}{hydflood}
#' 
"spdf.tiles_rhein"
