
# store spdf.active_floodplain_elbe_csa as external dataset
if (!(file.exists("data-raw/spdf.active_floodplain_elbe_csa.rda"))) {
    
    # load missing dataset
    if (! exists("spdf.tiles_elbe")) {
        load("data/spdf.tiles_elbe.rda")
    }
    
    # initialize GrassGIS
    gg_gd <- paste0(Sys.getenv("HOME"), "/GrassGIS")
    gg_ln <- "ELBE_Binnen"
    gg_ma <- "PERMANENT"
    library("rgrass7")
    library("sp")
    library("sf")
    initGRASS(gisBase = "/opt/i4/grassgis-7.8.5/grass78",
              gisDbase = gg_gd,
              location = gg_ln,
              mapset = gg_ma,
              override = TRUE,
              remove_GISRC = TRUE)
    use_sp()
    execGRASS("g.proj", flags = "c", epsg = 25833)
    Sys.setenv(PYTHONPATH = paste0("/opt/i4/grassgis-7.8.5/grass78/etc/python:",
                                   "/opt/i4/i4-0.0.6/lib/python3.8/site-packag",
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
    cache_dem <- paste0(Sys.getenv("HOME"), "/.hydflood/", spdf.tiles_elbe$name,
                        "_DEM.tif")
    cache_csa <- paste0(Sys.getenv("HOME"), "/.hydflood/", spdf.tiles_elbe$name,
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
    }
    
    # import DEM's and compute CSA's
    for (i in 1:length(spdf.tiles_elbe)) {
        # create or change into a mapset
        if (! dir.exists(paste(sep = "/", gg_gd, gg_ln,
                               spdf.tiles_elbe$name[i]))) {
            execGRASS("g.mapset", flags = c("c", "quiet"),
                      mapset = spdf.tiles_elbe$name[i], location = gg_ln)
        } else {
            execGRASS("g.mapset", flags = c("quiet"),
                      mapset = spdf.tiles_elbe$name[i], location = gg_ln)
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
                                                  spdf.tiles_elbe$name[i],
                                                  "@PERMANENT"))
        }
        
        if (! paste0(substr(spdf.tiles_elbe$name[i], 1, 5),
                     "cross_section_areas") %in% vectors_present_m) {
            execGRASS("r.to.vect", input = "CROSS_SECTION_AREAS",
                      output = paste0(substr(spdf.tiles_elbe$name[i], 1, 5),
                                      "cross_section_areas"),
                      type = "area", column = "station_int",
                      flags = c("quiet", "overwrite"))
            execGRASS("v.extract",
                      input = paste0(substr(spdf.tiles_elbe$name[i], 1, 5),
                                            "cross_section_areas"),
                      output = paste0(substr(spdf.tiles_elbe$name[i], 1, 5),
                                      "cross_section_areas_sel"),
                      where = paste0("station_int >= ",
                                     df.sections_elbe$from_km[i] * 1000,
                                     " AND station_int < ",
                                     df.sections_elbe$to_km[i] * 1000),
                      flags = c("quiet", "overwrite"))
            execGRASS("g.rename", flags = c("quiet", "overwrite"),
                      vector = paste0(substr(spdf.tiles_elbe$name[i], 1, 5),
                                      "cross_section_areas_sel,",
                                      substr(spdf.tiles_elbe$name[i], 1, 5),
                                      "cross_section_areas"))
        }
    }
    
    execGRASS("g.mapset", flags = c("c", "quiet"), mapset = gg_ma,
              location = gg_ln)
    
    if (! "cross_section_areas" %in% vectors_present) {
        execGRASS("v.patch", flags = c("quiet", "e", "overwrite"),
                  input = paste0(
                      paste0(substr(spdf.tiles_elbe$name, 1, 5),
                             "cross_section_areas@", spdf.tiles_elbe$name),
                      collapse = ","),
                  output = "cross_section_areas")
        execGRASS("v.dissolve", input = "cross_section_areas",
                  column = "station_int", output = "cross_section_areas",
                  flags = "overwrite")
    }
    
    spdf.active_floodplain_elbe_csa <- readVECT("cross_section_areas@PERMANENT")
    crs(spdf.active_floodplain_elbe_csa) <- UTM33N
    names(spdf.active_floodplain_elbe_csa) <- "station_int"
    spdf.active_floodplain_elbe_csa$station <- as.numeric(
        spdf.active_floodplain_elbe_csa$station_int / 1000)
    
    # section
    if (!"section" %in% names(spdf.active_floodplain_elbe_csa)) {
        spdf.tiles <- spdf.tiles_elbe[,c("name")]
        names(spdf.tiles) <- "section"
        sf.af <- st_as_sf(spdf.active_floodplain_elbe_csa)
        sf.t <- st_as_sf(spdf.tiles)
        sf.aft <- st_join(sf.af, sf.t)
        sf.aft <- sf.aft[!grepl(".", row.names(sf.aft), fixed = TRUE), ]
        spdf.active_floodplain_elbe_csa <- as(sf.aft, "Spatial")
        rm(spdf.tiles, sf.af, sf.t, sf.aft)
    }
    
    spdf.active_floodplain_elbe_csa$section <- 
        as.character(spdf.active_floodplain_elbe_csa$section)
    
    # section_do
    if (!"section_do" %in% names(spdf.active_floodplain_elbe_csa)) {
        
        spdf.active_floodplain_elbe_csa$section_do <- 
            rep(NA_character_, nrow(spdf.active_floodplain_elbe_csa))
        
        spdf.tiles <- spdf.tiles_elbe[,c("name")]
        names(spdf.tiles) <- "section"
        
        for (i in 1:(nrow(spdf.tiles) - 1)) {
            # in section
            section <- spdf.tiles$section[i]
            id_sec <- row.names(spdf.active_floodplain_elbe_csa[
                which(spdf.active_floodplain_elbe_csa$section == section), ])
            
            # in downstream tile
            section_do <- spdf.tiles$section[i + 1]
            spdf.tile_sel <- spdf.tiles[i + 1,]
            id_sel <- row.names(spdf.active_floodplain_elbe_csa[spdf.tile_sel,])
            
            id <- id_sec[id_sec %in% id_sel]
            spdf.active_floodplain_elbe_csa[id, "section_do"] <- as.character(section_do)
        }
        rm(i)
    }
    
    # bwstr_id
    if (!"bwastr_id" %in% names(spdf.active_floodplain_elbe_csa)) {
        spdf.hecto <- readOGR("/home/WeberA/praktikum/data-raw/VerkNetBWaStr",
                              "hectometer")[, 2]
        crs(spdf.hecto) <- UTM33N
        names(spdf.hecto) <- "bwastr_id"
        sf.af <- st_as_sf(spdf.active_floodplain_elbe_csa)
        sf.t <- st_as_sf(spdf.hecto)
        sf.aft <- st_join(sf.af, sf.t)
        sf.aft <- sf.aft[!grepl(".", row.names(sf.aft), fixed = TRUE), ]
        spdf.active_floodplain_elbe_csa <- as(sf.aft, "Spatial")
        rm(spdf.hecto, sf.af, sf.t, sf.aft)
    }
    
    spdf.active_floodplain_elbe_csa$bwastr_id <- 
        as.character(spdf.active_floodplain_elbe_csa$bwastr_id)
    
    # reorder columns
    spdf.active_floodplain_elbe_csa <- spdf.active_floodplain_elbe_csa[ ,
        c("bwastr_id", "station_int", "station", "section", "section_do")]
    
    # export
    usethis::use_data(spdf.active_floodplain_elbe_csa,
                      overwrite = TRUE, compress = "bzip2")
    system("mv data/spdf.active_floodplain_elbe_csa.rda data-raw/")
    system("cp data-raw/spdf.active_floodplain_elbe_csa.rda ~/.hydflood/")
    
    # clean up
    rm(spdf.active_floodplain_elbe_csa, spdf.tiles, spdf.tile_sel,
       cache_csa, cache_dem, df.sections_elbe, id, id_sec, id_sel,
       rasters_present, rasters_present_m, section, section_do, vectors_present,
       vectors_present_m, gg_gd, gg_ln, gg_ma, spdf.tiles_elbe)
    
} else {
    write("data-raw/spdf.active_floodplain_elbe_csa.rda exists already", 
          stderr())
}

