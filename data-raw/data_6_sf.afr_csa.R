
# store sf.afr_csa as external dataset
if (!(file.exists("data-raw/sf.afr_csa.rda"))) {
    
    # load missing dataset
    if (! exists("sf.tiles_rhine")) {
        load("data/sf.tiles_rhine.rda")
    }
    
    # initialize GrassGIS
    gg_gd <- paste0(Sys.getenv("HOME"), "/GrassGIS")
    gg_ln <- "RHEIN"
    gg_ma <- "PERMANENT"
    library("rgrass7")
    library("tidyverse")
    initGRASS(gisBase = "/opt/i4/grassgis-8.0.2/grass80",
              gisDbase = gg_gd,
              location = gg_ln,
              mapset = gg_ma,
              override = TRUE,
              remove_GISRC = TRUE)
    use_sf()
    execGRASS("g.proj", flags = "c", epsg = 25832)
    Sys.setenv(PYTHONPATH = paste0("/opt/i4/grassgis-8.0.2/grass80/etc/python:",
                                   "/opt/i4/i4-0.0.8/lib/python3.10/site-packa",
                                   "ges:/opt/i4/python-3.10.5/lib/python3.10/s",
                                   "ite-packages"))
    
    # df.sections
    df.sections_rhine <- hyd1d::df.sections[
        which(hyd1d::df.sections$river == "RHINE"),]
    
    # import
    rasters_present <- rgrass7::execGRASS("g.list", mapset = ".", 
                                          type = "raster", intern = TRUE)
    vectors_present <- rgrass7::execGRASS("g.list", mapset = ".", 
                                          type = "vector", intern = TRUE)
    
    # files
    cache_dem <- paste0(Sys.getenv("HOME"), "/.hydflood/", sf.tiles_rhine$name,
                        "_DEM.tif")
    cache_csa <- paste0(Sys.getenv("HOME"), "/.hydflood/", sf.tiles_rhine$name,
                        "_CSA.tif")
    
    # import missing vector data
    if (! "active_floodplain" %in% vectors_present) {
        execGRASS("v.import", input = "data-raw/active_floodplain_rhine.shp",
                  output = "active_floodplain", encoding = "UTF-8")
    }
    if (! "tiles" %in% vectors_present) {
        execGRASS("v.import", input = "data-raw/tiles_rhine.shp",
                  output = "tiles", encoding = "UTF-8")
    }
    if (! "cross_section_traces" %in% vectors_present) {
        execGRASS("v.import", input = "data-raw/cross_section_traces_rhine.shp",
                  output = "cross_section_traces", encoding = "UTF-8")
        execGRASS("v.db.renamecolumn", map = "cross_section_traces",
                  column = "station_in,station_int")
    }
    
    # import DEM's and compute CSA's
    for (i in 1:nrow(sf.tiles_rhine)) {
        # create or change into a mapset
        if (! dir.exists(paste(sep = "/", gg_gd, gg_ln,
                               sf.tiles_rhine$name[i]))) {
            execGRASS("g.mapset", flags = c("c", "quiet"),
                      mapset = sf.tiles_rhine$name[i], location = gg_ln)
        } else {
            execGRASS("g.mapset", flags = c("quiet"),
                      mapset = sf.tiles_rhine$name[i], location = gg_ln)
        }
        
        # import the DEM's
        rasters_present_m <- rgrass7::execGRASS("g.list", mapset = ".", 
                                                type = "raster", intern = TRUE)
        vectors_present_m <- rgrass7::execGRASS("g.list", mapset = ".", 
                                                type = "vector", intern = TRUE)
        if (! "DEM" %in% rasters_present_m) {
            execGRASS("r.in.gdal", flags = c("quiet", "overwrite"),
                      input = cache_dem[i], output = "DEM")
        }
        if (! "ACTIVE_FLOODPLAIN"  %in% rasters_present_m) {
            execGRASS("v.to.rast", flags = c("quiet", "overwrite"),
                      input = "active_floodplain@PERMANENT",
                      output = "ACTIVE_FLOODPLAIN", use = "val")
        }
        if (! "CROSS_SECTION_TRACES"  %in% rasters_present_m) {
            execGRASS("v.to.rast", flags = c("quiet", "overwrite"),
                      input = "cross_section_traces@PERMANENT",
                      output = "CROSS_SECTION_TRACES", use = "attr",
                      attribute_column = "station_int")
        }
        if (! "CROSS_SECTION_AREAS"  %in% rasters_present_m) {
            execGRASS("r.grow.distance", flags = c("quiet", "overwrite"),
                      input = "CROSS_SECTION_TRACES",
                      value = "CROSS_SECTION_AREAS")
            execGRASS("r.mask", raster = "ACTIVE_FLOODPLAIN",
                      flags = "overwrite")
            execGRASS("r.mapcalc", flags = c("quiet", "overwrite"),
                      expression = "CROSS_SECTION_AREAS=int(CROSS_SECTION_AREAS)")
            execGRASS("r.mask", "r")
        }
        
        if (! file.exists(cache_csa[i])) {
            execGRASS("g.region", raster = "DEM")
            execGRASS("r.out.gdal", input = "CROSS_SECTION_AREAS",
                      type = "Int32", nodata=-999, flags = c("c", "f", "overwrite"),
                      output = cache_csa[i], createopt = c("TFW=NO",
                                                           "COMPRESS=LZW"))
            execGRASS("g.region", region = paste0("region_",
                                                  sf.tiles_rhine$name[i],
                                                  "@PERMANENT"))
        }
        
        if (! paste0(substr(sf.tiles_rhine$name[i], 1, 5),
                     "cross_section_areas") %in% vectors_present_m) {
            execGRASS("r.to.vect", input = "CROSS_SECTION_AREAS",
                      output = paste0(substr(sf.tiles_rhine$name[i], 1, 5),
                                      "cross_section_areas"),
                      type = "area", column = "station_int",
                      flags = c("quiet", "overwrite"))
            execGRASS("v.extract",
                      input = paste0(substr(sf.tiles_rhine$name[i], 1, 5),
                                     "cross_section_areas"),
                      output = paste0(substr(sf.tiles_rhine$name[i], 1, 5),
                                      "cross_section_areas_sel"),
                      where = paste0("station_int >= ",
                                     df.sections_rhine$from_km[i] * 1000,
                                     " AND station_int < ",
                                     df.sections_rhine$to_km[i] * 1000),
                      flags = c("quiet", "overwrite"))
            execGRASS("g.rename", flags = c("quiet", "overwrite"),
                      vector = paste0(substr(sf.tiles_rhine$name[i], 1, 5),
                                      "cross_section_areas_sel,",
                                      substr(sf.tiles_rhine$name[i], 1, 5),
                                      "cross_section_areas"))
        }
    }
    
    execGRASS("g.mapset", flags = c("c", "quiet"), mapset = gg_ma,
              location = gg_ln)
    
    if (! "cross_section_areas" %in% vectors_present) {
        execGRASS("v.patch", flags = c("quiet", "e", "overwrite"),
                  input = paste0(
                      paste0(substr(sf.tiles_rhine$name, 1, 5),
                             "cross_section_areas@", sf.tiles_rhine$name),
                      collapse = ","),
                  output = "cross_section_areas")
        execGRASS("v.dissolve", input = "cross_section_areas",
                  column = "station_int", output = "cross_section_areas",
                  flags = "overwrite")
    }
    
    sf.afr_csa <- readVECT("cross_section_areas@PERMANENT")
    names(sf.afr_csa)[1] <- "station_int"
    sf.afr_csa$station <- as.numeric(sf.afr_csa$station_int / 1000)
    sf.afr_csa <- sf.afr_csa %>%
        st_cast("POLYGON")  %>%
        aggregate(list(.$station_int), first) %>%
        st_cast("MULTIPOLYGON")
    
    # section
    if (!"section" %in% names(sf.afr_csa)) {
        sf.tiles <- sf.tiles_rhine[, "name"]
        names(sf.tiles)[1] <- "section"
        sf.afr_csa <- st_join(sf.afr_csa, sf.tiles)
        sf.afr_csa <- sf.afr_csa[!grepl(".", row.names(sf.afr_csa),
                                        fixed = TRUE), ]
        rm(sf.tiles)
    }
    
    sf.afr_csa$section <- as.character(sf.afr_csa$section)
    
    # section_do
    if (!"section_do" %in% names(sf.afr_csa)) {
        
        sf.afr_csa$section_do <- rep(NA_character_, nrow(sf.afr_csa))
        
        sf.tiles <- sf.tiles_rhine[,c("name")]
        names(sf.tiles)[1] <- "section"
        
        for (i in 1:(nrow(sf.tiles) - 1)) {
            # in section
            section <- sf.tiles$section[i]
            id_sec <- row.names(sf.afr_csa[which(sf.afr_csa$section == section), ])
            
            # in downstream tile
            section_do <- sf.tiles$section[i + 1]
            sf.tile_sel <- sf.tiles[i + 1, ]
            id_sel <- row.names(sf.afr_csa[sf.tile_sel, ])
            
            id <- id_sec[id_sec %in% id_sel]
            sf.afr_csa[id, "section_do"] <- as.character(section_do)
        }
        rm(i, id, id_sec, id_sel, sf.tile_sel, sf.tiles, section, section_do)
    }
    
    sf.afr_csa$section_do <- as.character(sf.afr_csa$section_do)
    
    # bwstr_id
    if (!"bwastr_id" %in% names(sf.afr_csa)) {
        sf.afr_csa$bwastr_id <- "3901"
    }
    
    sf.afr_csa$bwastr_id <- as.character(sf.afr_csa$bwastr_id)
    
    # station_true
    
    # reorder columns
    sf.afr_csa <- sf.afr_csa[ , c("bwastr_id", "station_int", "station",
                                  "section", "section_do")]
    
    usethis::use_data(sf.afr_csa, overwrite = TRUE, compress = "bzip2")
    system("mv data/sf.afr_csa.rda data-raw/")
    system("cp data-raw/sf.afr_csa.rda ~/.hydflood/")
    
    # clean up
    rm(sf.afr_csa, cache_csa, cache_dem, df.sections_rhine, sf.tiles_rhine,
       rasters_present, rasters_present_m, vectors_present, vectors_present_m,
       gg_gd, gg_ln, gg_ma)
    
} else {
    write("data-raw/sf.afr_csa.rda exists already", 
          stderr())
}
