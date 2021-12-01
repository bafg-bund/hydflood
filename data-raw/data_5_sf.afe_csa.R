
# store sf.afe_csa as external dataset
if (!(file.exists("data-raw/sf.afe_csa.rda"))) {
    
    # load missing dataset
    if (! exists("sf.tiles_elbe")) {
        load("data/sf.tiles_elbe.rda")
    }
    
    # initialize GrassGIS
    gg_gd <- paste0(Sys.getenv("HOME"), "/GrassGIS")
    gg_ln <- "ELBE_Binnen"
    gg_ma <- "PERMANENT"
    library("rgrass7")
    library("tidyverse")
    initGRASS(gisBase = "/opt/i4/grassgis-7.8.5/grass78",
              gisDbase = gg_gd,
              location = gg_ln,
              mapset = gg_ma,
              override = TRUE,
              remove_GISRC = TRUE)
    use_sf()
    execGRASS("g.proj", flags = "c", epsg = 25833)
    Sys.setenv(PYTHONPATH = paste0("/opt/i4/grassgis-7.8.5/grass78/etc/python:",
                                   "/opt/i4/i4-0.0.7/lib/python3.8/site-packag",
                                   "es:/opt/i4/python-3.8.5/lib/python3.8/site",
                                   "-packages"))
    
    # df.sections
    df.sections_elbe <- hyd1d::df.sections[
        which(hyd1d::df.sections$river == "ELBE"),]
    
    # import
    rasters_present <- rgrass7::execGRASS("g.list", mapset = ".", 
                                           type = "raster", intern = TRUE)
    vectors_present <- rgrass7::execGRASS("g.list", mapset = ".", 
                                          type = "vector", intern = TRUE)
    
    # files
    cache_dem <- paste0(Sys.getenv("HOME"), "/.hydflood/", sf.tiles_elbe$name,
                        "_DEM.tif")
    cache_csa <- paste0(Sys.getenv("HOME"), "/.hydflood/", sf.tiles_elbe$name,
                        "_CSA.tif")
    
    # import missing vector data
    if (! "active_floodplain" %in% vectors_present) {
        execGRASS("v.import", input = "data-raw/active_floodplain_elbe.shp",
                  output = "active_floodplain", encoding = "UTF-8")
    }
    if (! "tiles" %in% vectors_present) {
        execGRASS("v.import", input = "data-raw/tiles_elbe.shp",
                  output = "tiles", encoding = "UTF-8")
    }
    if (! "cross_section_traces" %in% vectors_present) {
        execGRASS("v.import", input = "data-raw/cross_section_traces_elbe.shp",
                  output = "cross_section_traces", encoding = "UTF-8")
        execGRASS("v.db.renamecolumn", map = "cross_section_traces",
                  column = "station_in,station_int")
    }
    
    # import DEM's and compute CSA's
    for (i in 1:nrow(sf.tiles_elbe)) {
        # create or change into a mapset
        if (! dir.exists(paste(sep = "/", gg_gd, gg_ln,
                               sf.tiles_elbe$name[i]))) {
            execGRASS("g.mapset", flags = c("c", "quiet"),
                      mapset = sf.tiles_elbe$name[i], location = gg_ln)
        } else {
            execGRASS("g.mapset", flags = c("quiet"),
                      mapset = sf.tiles_elbe$name[i], location = gg_ln)
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
                                                  sf.tiles_elbe$name[i],
                                                  "@PERMANENT"))
        }
        
        if (! paste0(substr(sf.tiles_elbe$name[i], 1, 5),
                     "cross_section_areas") %in% vectors_present_m) {
            execGRASS("r.to.vect", input = "CROSS_SECTION_AREAS",
                      output = paste0(substr(sf.tiles_elbe$name[i], 1, 5),
                                      "cross_section_areas"),
                      type = "area", column = "station_int",
                      flags = c("quiet", "overwrite"))
            execGRASS("v.extract",
                      input = paste0(substr(sf.tiles_elbe$name[i], 1, 5),
                                            "cross_section_areas"),
                      output = paste0(substr(sf.tiles_elbe$name[i], 1, 5),
                                      "cross_section_areas_sel"),
                      where = paste0("station_int >= ",
                                     df.sections_elbe$from_km[i] * 1000,
                                     " AND station_int < ",
                                     df.sections_elbe$to_km[i] * 1000),
                      flags = c("quiet", "overwrite"))
            execGRASS("g.rename", flags = c("quiet", "overwrite"),
                      vector = paste0(substr(sf.tiles_elbe$name[i], 1, 5),
                                      "cross_section_areas_sel,",
                                      substr(sf.tiles_elbe$name[i], 1, 5),
                                      "cross_section_areas"))
        }
    }
    
    execGRASS("g.mapset", flags = c("c", "quiet"), mapset = gg_ma,
              location = gg_ln)
    
    if (! "cross_section_areas" %in% vectors_present) {
        execGRASS("v.patch", flags = c("quiet", "e", "overwrite"),
                  input = paste0(
                      paste0(substr(sf.tiles_elbe$name, 1, 5),
                             "cross_section_areas@", sf.tiles_elbe$name),
                      collapse = ","),
                  output = "cross_section_areas")
        execGRASS("v.dissolve", input = "cross_section_areas",
                  column = "station_int", output = "cross_section_areas",
                  flags = "overwrite")
    }
    
    sf.afe_csa <- readVECT("cross_section_areas@PERMANENT")
    sf.afe_csa <- sf.afe_csa %>%
                      st_cast("POLYGON")  %>%
                      aggregate(list(.$station_int), first) %>%
                      st_cast("MULTIPOLYGON")
    
    # section
    if (!"section" %in% names(sf.afe_csa)) {
        sf.tiles <- sf.tiles_elbe[, "name"]
        names(sf.tiles)[1] <- "section"
        sf.afe_csa <- st_join(sf.afe_csa, sf.tiles)
        sf.afe_csa <- sf.afe_csa[!grepl(".", row.names(sf.afe_csa),
                                        fixed = TRUE), ]
        rm(sf.tiles)
    }
    
    sf.afe_csa$section <- as.character(sf.afe_csa$section)
    
    # section_do
    if (!"section_do" %in% names(sf.afe_csa)) {
        
        sf.afe_csa$section_do <- rep(NA_character_, nrow(sf.afe_csa))
        
        sf.tiles <- sf.tiles_elbe[,c("name")]
        names(sf.tiles)[1] <- "section"
        
        for (i in 1:(nrow(sf.tiles) - 1)) {
            # in section
            section <- sf.tiles$section[i]
            id_sec <- row.names(sf.afe_csa[which(sf.afe_csa$section == section), ])
            
            # in downstream tile
            section_do <- sf.tiles$section[i + 1]
            sf.tile_sel <- sf.tiles[i + 1, ]
            id_sel <- row.names(sf.afe_csa[sf.tile_sel, ])
            
            id <- id_sec[id_sec %in% id_sel]
            sf.afe_csa[id, "section_do"] <- as.character(section_do)
        }
        rm(i, id, id_sec, id_sel, sf.tile_sel, sf.tiles, section, section_do)
    }
    
    sf.afe_csa$section_do <- as.character(sf.afe_csa$section_do)
    
    # bwstr_id
    if (!"bwastr_id" %in% names(sf.afe_csa)) {
        sf.hecto <- st_zm(st_read("/home/WeberA/elbe1d/data-raw/VerkNetBWaStr/hectometer.shp"))
        sf.hecto <- sf.hecto[, "BWASTR_ID"]
        names(sf.hecto)[1] <- "bwastr_id"
        sf.afe_csa <- st_join(sf.afe_csa, sf.hecto)
        rm(sf.hecto)
    }
    
    sf.afe_csa$bwastr_id <- as.character(sf.afe_csa$bwastr_id)
    
    # station_true
    if (!"station_true" %in% names(sf.afe_csa)) {
        load("~/.hydflood/spdf.afe_csa.rda")
        i <- order(spdf.afe_csa$station, spdf.afe_csa$bwastr_id)
        spdf.afe_csa <- spdf.afe_csa[i, ]
        
        sf.afe_csa$station_true <- spdf.afe_csa$station_true
        
        rm(i, spdf.afe_csa)
    }
    
    # reorder columns
    sf.afe_csa <- sf.afe_csa[ ,
        c("bwastr_id", "station_int", "station", "station_true", "section",
          "section_do")]
    
    # export
    usethis::use_data(sf.afe_csa, overwrite = TRUE, compress = "bzip2")
    system("mv data/sf.afe_csa.rda data-raw/")
    system("cp data-raw/sf.afe_csa.rda ~/.hydflood/")
    
    # clean up
    rm(sf.afe_csa, cache_csa, cache_dem, df.sections_elbe, sf.tiles_elbe,
       rasters_present, rasters_present_m, vectors_present, vectors_present_m,
       gg_gd, gg_ln, gg_ma)
    
} else {
    write("data-raw/sf.afe_csa.rda exists already", 
          stderr())
}

