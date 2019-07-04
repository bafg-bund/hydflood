################################################################################
# server.R
################################################################################

function(input, output, session) {
    
    # reactive values to control the UI and finally the computation
    res <- reactiveValues(
        river = NA_character_, 
        from_to = c(NA_real_, NA_real_), 
        river_from_to = c(NA_real_, NA_real_), 
        extent = c(NA_real_, NA_real_, NA_real_, NA_real_), 
        crs = NA_character_, 
        seq_from_to = c(NA_character_, NA_character_), 
        df.gd = data.frame(), 
        gs = NA_character_,
        email = NA_character_, 
        random = NA_character_,
        restored = FALSE
    )
    
    output$map <- renderLeaflet({
        leaflet() %>% addTiles() %>% 
            setMaxBounds(lng1 = min(c(df.coor.afe$lon, df.coor.afr$lon)) - 2, 
                         lat1 = min(c(df.coor.afe$lat, df.coor.afr$lat)) - 2, 
                         lng2 = max(c(df.coor.afe$lon, df.coor.afr$lon)) + 2, 
                         lat2 = max(c(df.coor.afe$lat, df.coor.afr$lat)) + 2) %>%
            fitBounds(lng1 = min(c(df.coor.afe$lon, df.coor.afr$lon)),
                      lat1 = min(c(df.coor.afe$lat, df.coor.afr$lat)),
                      lng2 = max(c(df.coor.afe$lon, df.coor.afr$lon)),
                      lat2 = max(c(df.coor.afe$lat, df.coor.afr$lat)))
    })
    
    #####
    # create menu item 1
    observe({
        if (is.na(res$river)) {
            output$river <- renderUI(
                selectInput(
                    inputId  = "river",
                    label    = "Fluss:",
                    choices  = rivers,
                    selected = "Bitte wählen Sie!"
                )
            )
        } else {
            if (res$restored){
                output$river <- renderUI(
                    disabled(
                        selectInput(
                            inputId  = "river",
                            label    = "Fluss:",
                            choices  = rivers,
                            selected = res$river
                        )
                    )
                )
            } else {
                output$river <- renderUI(
                    selectInput(
                        inputId  = "river",
                        label    = "Fluss:",
                        choices  = rivers,
                        selected = res$river
                    )
                )
            }
        }
    }, priority = 10)
    
    #####
    # respond to menu item 1: input$river and set res$river
    ###
    # set res$river
    observeEvent(input$river, {
        if (input$river != "Bitte wählen Sie!") {
            res$river <- input$river
        } else {
            res$river <- NA_character_
        }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    ###
    # responsive datasets
    ##
    # responsive df.from_to
    df.from_to_reactive <- reactive({
        if (is.na(res$river)) {
            return(df.from_to[FALSE, ])
        } else {
            return(df.from_to[which(df.from_to$river == res$river), ])
        }
    })
    
    # responsive crs
    crs_reactive <- reactive({
        if (is.na(res$river)) {
            return(NULL)
        } else {
            if (res$river == "Elbe") {
                return(CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"))
            } else {
                return(CRS("+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs"))
            }
        }
    })
    
    crs_comp_reactive <- reactive({
        if (is.na(res$river)) {
            return(NULL)
        } else {
            return(df.crs_comp$crs[which(df.crs_comp$river == res$river)])
        }
    })
    
    # responsive active floodplains in WGS 1984
    spdf.af_reactive <- reactive({
        if (is.na(res$river)) {
            return(NULL)
        } else {
            if (res$river == "Elbe") {
                return(spdf.afe)
            } else {
                return(spdf.afr)
            } 
        }
    })
    
    # responsive active floodplains in ETRS
    spdf.af_etrs_reactive <- reactive({
        if (is.na(res$river)) {
            return(NULL)
        } else {
            if (res$river == "Elbe") {
                return(spdf.active_floodplain_elbe)
            } else {
                return(spdf.active_floodplain_rhein)
            } 
        }
    })
    
    # responsive spdf.gsd
    spdf.gsd_reactive <- reactive({
        if (is.na(res$river)) {
            return(NULL)
        } else {
            return(spdf.gsd[which(spdf.gsd$river == 
                                      toupper(res$river)), ])
        }
    })
    
    ###
    # observe changes of res$river and depending elements
    ##
    # active floodplain polygons
    observe({
        
        l <- leafletProxy("map")
        
        if (is.na(res$river)) {
            l %>% addPolygons(lng = df.coor.afe$lon, lat = df.coor.afe$lat,
                              label = "Elbe", color = "blue", weight = 2,
                              fill = TRUE, fillColor = "lightblue", 
                              fillOpacity = 0.6, layerId = "afe")
            l %>% addPolygons(lng = df.coor.afr$lon, lat = df.coor.afr$lat,
                              label = "Rhein", color = "blue", weight = 2,
                              fill = TRUE, fillColor = "lightblue", 
                              fillOpacity = 0.6, layerId = "afr")
        } else {
            if (res$river == "Elbe") {
                l %>% removeShape(layerId = c("afr"))
                l %>% addPolygons(lng = df.coor.afe$lon, lat = df.coor.afe$lat,
                                  label = "Elbe", color = "blue", weight = 2,
                                  fill = TRUE, fillColor = "lightblue", 
                                  fillOpacity = 0.6, layerId = "afe")
            } else {
                l %>% removeShape(layerId = c("afe"))
                l %>% addPolygons(lng = df.coor.afr$lon, lat = df.coor.afr$lat,
                                  label = "Rhein", color = "blue", weight = 2,
                                  fill = TRUE, fillColor = "lightblue", 
                                  fillOpacity = 0.6, layerId = "afr")
            }
        }
    }, priority = 5)
    
    ##
    # gauging stations
    observe({
        
        l <- leafletProxy("map")
        l %>% clearGroup(group = "gs")
        
        if (!is.na(res$river)){
            l %>% addCircles(lng = ~longitude, 
                             lat = ~latitude, 
                             label = htmlEscape(~gauging_station), 
                             popup = htmlEscape(~gauging_station), 
                             group = "gs", color = "black", opacity = 1, 
                             fillColor = "yellow", fillOpacity = 1, 
                             data = spdf.gsd_reactive())
        }
    }, priority = 5)
    
    #####
    # create or update the basic settings of menu item 2
    observe({
        if (is.na(res$river)) {
            output$from_to <- renderUI("")
        } else {
            if (res$restored) {
                output$from_to <- renderUI({
                    disabled(
                        sliderInput(
                            inputId = "from_to",
                            label   = "Abschnitt (von km - bis km):",
                            min     = res$river_from_to[1],
                            max     = res$river_from_to[2],
                            value   = c(res$river_from_to[1], 
                                        res$river_from_to[2]),
                            step    = 0.1
                        )
                    )
                })
            } else {
                res$river_from_to <- c(df.from_to_reactive()$from_val, 
                                       df.from_to_reactive()$to_val) 
                output$from_to <- renderUI({
                    sliderInput(
                        inputId = "from_to",
                        label   = "Abschnitt (von km - bis km):",
                        min     = res$river_from_to[1],
                        max     = res$river_from_to[2],
                        value   = c(res$river_from_to[1], 
                                    res$river_from_to[2]),
                        step    = 0.1
                    )
                })
            }
        }
    }, priority = 10)
    
    #####
    # respond to menu item 1: input$river and set res$river
    ###
    # set res$river
    observeEvent(input$from_to, {
        res$from_to <- input$from_to
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    ###
    # responsive datasets
    ##
    # responsive spdf.station
    spdf.station_reactive <- reactive({
        if (is.na(res$river)) {
            return(spdf.station[FALSE, ])
        } else {
            if (!all(is.na(res$from_to))) {
                return(spdf.station[
                    which(spdf.station$river == res$river &
                              spdf.station$station >= res$from_to[1] &
                              spdf.station$station <= res$from_to[2]), ])
            } else {
                return(spdf.station)
            }
        }
    })
    
    ###
    # observe changes of input$from_to
    ##
    # modify background's extent
    observeEvent(spdf.station_reactive(), {
        
        l <- leafletProxy("map")
        
        if (nrow(spdf.station_reactive()) != 0) {
            l %>% fitBounds(extent(spdf.station_reactive())@xmin,
                            extent(spdf.station_reactive())@ymin,
                            extent(spdf.station_reactive())@xmax,
                            extent(spdf.station_reactive())@ymax)
        }
    })
    
    # reactive boolean, wether drawing toolbar should be enabled
    draw_reactive <- reactive({
        if (is.na(res$river)) {
            return(FALSE)
        } else {
           if (!all(is.na(res$from_to)) & !res$restored) {
                return(TRUE)
            } else {
                return(FALSE)
            }
        }
    })
    
    # add or remove the drawing tool
    observe({
        if (draw_reactive()) {
            output$area <- renderUI({
                tagList(h4("Berechnungsgebiet"), 
                        p("Fläche von max. 2500 ha"),
                        p(paste0("Grenzen Sie das Berechnungsgebiet mit der Re",
                                 "chteckmaske oben links ein!")))
            })
            l <- leafletProxy("map")
            l %>% addDrawToolbar(
                circleOptions = FALSE, polygonOptions = FALSE,
                polylineOptions = FALSE, markerOptions = FALSE,
                circleMarkerOptions = FALSE, singleFeature = TRUE,
                editOptions = editToolbarOptions(
                    selectedPathOptions = selectedPathOptions(), remove = TRUE))
        } else {
            if (res$restored) {
                
                # capture ETRS extent
                extent_etrs <- extent(res$extent)
                sp.extent_etrs <- as(extent_etrs, "SpatialPolygons")
                crs(sp.extent_etrs) <- CRS(res$crs)
                
                output$area <- renderUI({
                    tagList(
                        h4("Berechnungsgebiet"),
                        p(paste0("Fläche: ", 
                                 round(area(sp.extent_etrs) / 10000, 2),
                                 " ha (", extent_etrs@xmax - extent_etrs@xmin, 
                                 " x ", extent_etrs@ymax - extent_etrs@ymin, 
                                 " m)")),
                        splitLayout(
                            disabled(numericInput("res_xmin", "xmin", 
                                                  extent_etrs@xmin)),
                            disabled(numericInput("res_xmax", "xmax", 
                                                  extent_etrs@xmax))),
                        splitLayout(
                            disabled(numericInput("res_ymin", "ymin", 
                                                  extent_etrs@ymin)),
                            disabled(numericInput("res_ymax", "ymax", 
                                                  extent_etrs@ymax))),
                        p("")
                    )
                })
            }
        }
    })
    
    ############################################################################
    # DRAWING
    #####
    # observe all features which are created/edited/deleted from the map
    observeEvent(input$map_draw_all_features, {
        
        # capture drawed features
        input.features <- input$map_draw_all_features$features
        
        # start assembling the output$area
        o <- tagList(h4("Berechnungsgebiet"), 
                     p("Fläche von max. 2500 ha"))
        
        if (length(input.features) < 1) {
            o[[length(o) + 1]] <- p(paste0("Grenzen Sie das Berechnungsgebiet ",
                                           "mit der Rechteckmaske oben links e",
                                           "in!"))
        } else if (length(input.features) == 1) {
            
            # convert the captured coordinates into a SpatialPolygons-object
            input.coor <- input.features[[1]]$geometry$coordinates[[1]]
            df.coor <- data.frame(lon = rep(NA_real_, 5), 
                                  lat = rep(NA_real_, 5))
            for (i in 1:length(input.coor)) {
                df.coor[i, ] <- c(input.coor[[i]][1], input.coor[[i]][2])
            }
            p.area <- Polygons(list(Polygon(coords = df.coor, 
                                            hole = FALSE)), "1")
            sp.area <- SpatialPolygons(list(p.area), proj4string = crs)
            
            # check, if sp.area is part of the active floodplain
            if (! in_af(sp.area, spdf.af_reactive())) {
                o[[length(o) + 1]] <- p(paste0("Die gewählte Fläche liegt voll",
                                               "ständig außerhalb der aktiven ",
                                               "Aue. Bitte passen Sie die Lage",
                                               " an!"))
            } else {
                # check the size
                if (area(sp.area) > 25000000) {
                    o[[length(o) + 1]] <- p(paste0("Die gewählte Fläche ist ", 
                                                   round(area(sp.area) / 10000, 
                                                         2), 
                                                   " ha groß und damit größer ",
                                                   "als die maximal erlaubten ",
                                                   "2500 ha. Bitte reduzieren ",
                                                   "Sie die Flächengröße!"))
                } else {
                    sp.area_etrs <- spTransform(sp.area, 
                                                CRSobj = crs_reactive())
                    extent_etrs <- extent(floor(extent(sp.area_etrs)@xmin),
                                          ceiling(extent(sp.area_etrs)@xmax),
                                          floor(extent(sp.area_etrs)@ymin),
                                          ceiling(extent(sp.area_etrs)@ymax))
                    sp.area_etrs_valid <- as(extent_etrs, 'SpatialPolygons')
                    crs(sp.area_etrs_valid) <- crs_reactive()
                    
                    # add a temporary polygon and zoom
                    sp.area_valid <- spTransform(sp.area_etrs_valid, crs)
                    ma <- sp.area_valid@polygons[[1]]@Polygons[[1]]@coords
                    e <- extent(sp.area_valid)
                    
                    l <- leafletProxy("map")
                    l %>% addPolygons(lng = ma[,1], lat = ma[,2],
                                      color = "black", weight = 2,
                                      fillColor = "lightblue", 
                                      fillOpacity = 0.7,
                                      layerId = "area_tmp")
                    l %>% fitBounds(lng1 = e@xmin - (e@xmax - e@xmin) / 3,
                                    lat1 = e@ymin - (e@ymax - e@ymin) / 3,
                                    lng2 = e@xmax + (e@xmax - e@xmin) / 3,
                                    lat2 = e@ymax + (e@ymax - e@ymin) / 3)
                    
                    # generate output
                    o[[length(o) + 1]] <- p(paste0("Die gewählte Fläche ist ", 
                                                   round(area(sp.area) 
                                                         / 10000, 2), " ha gro",
                                                   "ß (WGS84). Transformiert n", 
                                                   "ach ",crs_comp_reactive(), 
                                                   ", dem CRS in dem die Ber",
                                                   "echnung ausgeführt wird, i",
                                                   "st die Fläche ",
                                                   round(area(sp.area_valid) 
                                                         / 10000, 2), " ha gro",
                                                   "ß und hat folgende Ausdehn",
                                                   "ung:"))
                    o[[length(o) + 1]] <- p(paste0("x-Ausdehnung: ", 
                                                   extent_etrs@xmax - 
                                                       extent_etrs@xmin, " m"))
                    o[[length(o) + 1]] <- splitLayout(
                        numericInput("xmin", "xmin", 
                                     extent_etrs@xmin),
                        numericInput("xmax", "xmax", 
                                     extent_etrs@xmax))
                    o[[length(o) + 1]] <- p(paste0("y-Ausdehnung: ", 
                                                   extent_etrs@ymax - 
                                                       extent_etrs@ymin, " m"))
                    o[[length(o) + 1]] <- splitLayout(
                        numericInput("ymin", "ymin", 
                                     extent_etrs@ymin),
                        numericInput("ymax", "ymax", 
                                     extent_etrs@ymax))
                    o[[length(o) + 1]] <- p("")
                    o[[length(o) + 1]] <- splitLayout(
                        actionButton("area_modify", 
                                     "Gebiet ändern"),
                        actionButton("area_confirm", 
                                     "Gebiet bestätigen"))
                }
            }
        } else if (length(input.features) > 1) {
            o[[length(o) + 1]] <- p(paste0("Sie haben ", length(input.features),
                                           " Berechnungsgebiete definiert, es ",
                                           "ist aber maximal eins zulässig. Bi",
                                           "tte entfernen Sie überzählige Gebi",
                                           "ete!"))
        } else {
            o[[length(o) + 1]] <- p(paste0("Etwas stimmt nicht. Laden Sie die ",
                                           "App mit F5 bitte neu!"))
        }
        
        output$area <- renderUI({o})
    })
    
    # responsive data.frame with area coordinates
    df.coor_area_reactive <- reactive({
        if (!is.null(input$xmax) & !is.null(input$xmin) & 
            !is.null(input$ymax) & !is.null(input$ymin)) {
            if (input$xmax > input$xmin & input$ymax > input$ymin) {
                # capture ETRS extent
                extent_etrs <- extent(input$xmin, input$xmax, 
                                      input$ymin, input$ymax)
                sp.area_etrs <- as(extent_etrs, "SpatialPolygons")
                crs(sp.area_etrs) <- crs_reactive()
                
                # transform it to WGS84 and return the coordinates as data.frame
                sp.area <- spTransform(sp.area_etrs, CRSobj = crs)
                ma <- sp.area@polygons[[1]]@Polygons[[1]]@coords
                return(data.frame(lng = ma[, 1], lat = ma[, 2]))
            } else {
                return(NULL)
            }
        } else if (!(all(is.na(res$extent)))) {
            # capture ETRS extent
            extent_etrs <- extent(res$extent)
            sp.area_etrs <- as(extent_etrs, "SpatialPolygons")
            crs(sp.area_etrs) <- CRS(res$crs)
            
            # transform it to WGS84 and return the coordinates as data.frame
            sp.area <- spTransform(sp.area_etrs, CRSobj = crs)
            ma <- sp.area@polygons[[1]]@Polygons[[1]]@coords
            return(data.frame(lng = ma[, 1], lat = ma[, 2]))
        } else {
            return(NULL)
        }
    })
    
    # modify
    observeEvent(input$area_modify, {
        
        l <- leafletProxy("map")
        
        if (!is.null(df.coor_area_reactive())) {
            l %>% removeShape(layerId = c("area_tmp"))
            l %>% addPolygons(lng = df.coor_area_reactive()$lng, 
                              lat = df.coor_area_reactive()$lat,
                              color = "black", weight = 2,
                              fillColor = "lightblue", 
                              fillOpacity = 0.7,
                              layerId = "area_temp")
            e <- extent(
                SpatialPolygons(
                    list(Polygons(
                        list(Polygon(df.coor_area_reactive())), ID = 1)), 
                    proj4string = crs))
            l %>% fitBounds(lng1 = e@xmin - (e@xmax - e@xmin) / 3,
                            lat1 = e@ymin - (e@ymax - e@ymin) / 3,
                            lng2 = e@xmax + (e@xmax - e@xmin) / 3,
                            lat2 = e@ymax + (e@ymax - e@ymin) / 3)
        }
        
        o <- tagList(h4("Berechnungsgebiet"), 
                     p("Fläche von max. 2500 ha"))
        
        # validate xmax > xmin & ymax > ymin
        if (input$xmax <= input$xmin | input$ymax <= input$ymin) {
            if (input$xmax <= input$xmin) {
                o[[length(o) + 1]] <- p(paste0(strong("xmax <= xmin"), ": xmax",
                                               " muss größer als xmin sein."))
            }
            o[[length(o) + 1]] <- p(paste0("x-Ausdehnung: ", 
                                           input$xmax - input$xmin))
            o[[length(o) + 1]] <- splitLayout(numericInput("xmin", "xmin", 
                                                           input$xmin),
                                              numericInput("xmax", "xmax", 
                                                           input$xmax))
            if (input$ymax <= input$ymin) {
                o[[length(o) + 1]] <- p(paste0(strong("ymax <= ymin"), ": ymax",
                                               " muss größer als ymin sein."))
            }
            o[[length(o) + 1]] <- p(paste0("y-Ausdehnung: ", 
                                           input$ymax - input$ymin))
            o[[length(o) + 1]] <- splitLayout(numericInput("ymin", "ymin", 
                                                           input$ymin),
                                              numericInput("ymax", "ymax", 
                                                           input$ymax))
            o[[length(o) + 1]] <- p("")
            o[[length(o) + 1]] <- splitLayout(actionButton("area_modify", 
                                                           "Gebiet ändern"),
                                              p(""))
            o[[length(o) + 1]] <- p("")
            
        } else {
            
            # capture ETRS extent
            extent_etrs <- extent(input$xmin, input$xmax, input$ymin, 
                                  input$ymax)
            sp.extent_etrs <- as(extent_etrs, "SpatialPolygons")
            crs(sp.extent_etrs) <- crs_reactive()
            
            sp.extent <- spTransform(sp.extent_etrs, CRSobj = crs)
            
            # check, if it is part of the active floodplain
            if (! in_af(sp.extent_etrs, spdf.af_etrs_reactive())) {
                o[[length(o) + 1]] <- p(paste0("Die gewählte Fläche liegt voll",
                                               "ständig außerhalb der aktiven ",
                                               "Aue. Bitte passen Sie die Lage",
                                               " an!"))
                
                if (area(sp.extent_etrs) > 25000000) {
                    o[[length(o) + 1]] <- 
                        p(paste0("Die gewählte Fläche ist zudem ", 
                                 round(area(sp.extent_etrs) / 10000, 2), 
                                 " ha groß (", crs_comp_reactive(), 
                                 ") und damit größer als die maximal erlaubten",
                                 " 2500 ha . Bitte reduzieren Sie auch die Flä",
                                 "chengröße:"))
                    o[[length(o) + 1]] <- p(paste0("x-Ausdehnung: ", 
                                                   extent_etrs@xmax - 
                                                       extent_etrs@xmin, " m"))
                    o[[length(o) + 1]] <- splitLayout(
                        numericInput("xmin", "xmin", 
                                     extent_etrs@xmin),
                        numericInput("xmax", "xmax", 
                                     extent_etrs@xmax))
                    o[[length(o) + 1]] <- p(paste0("y-Ausdehnung: ", 
                                                   extent_etrs@ymax - 
                                                       extent_etrs@ymin, " m"))
                    o[[length(o) + 1]] <- splitLayout(
                        numericInput("ymin", "ymin", 
                                     extent_etrs@ymin),
                        numericInput("ymax", "ymax", 
                                     extent_etrs@ymax))
                    o[[length(o) + 1]] <- p("")
                    o[[length(o) + 1]] <- splitLayout(
                        actionButton("area_modify", 
                                     "Gebiet ändern"),
                        p(""))
                    o[[length(o) + 1]] <- p("")
                    
                } else if (area(sp.extent) <= 25000000) {
                    o[[length(o) + 1]] <- 
                        p(paste0("Die gewählte Fläche ist ", 
                                 round(area(sp.extent) / 10000, 2), " ha groß ",
                                 "(", crs_comp_reactive(), "). Sie brauchen be",
                                 "i den Änderungen also nur auf die Lage zu ak",
                                 "tiven Aue achten:"))
                    o[[length(o) + 1]] <- p(paste0("x-Ausdehnung: ", 
                                                   extent_etrs@xmax - 
                                                       extent_etrs@xmin, " m"))
                    o[[length(o) + 1]] <- splitLayout(
                        numericInput("xmin", "xmin", extent_etrs@xmin),
                        numericInput("xmax", "xmax", extent_etrs@xmax))
                    o[[length(o) + 1]] <- p(paste0("y-Ausdehnung: ", 
                                                   extent_etrs@ymax - 
                                                       extent_etrs@ymin, " m"))
                    o[[length(o) + 1]] <- splitLayout(
                        numericInput("ymin", "ymin", extent_etrs@ymin),
                        numericInput("ymax", "ymax", extent_etrs@ymax))
                    o[[length(o) + 1]] <- p("")
                    
                    o[[length(o) + 1]] <- splitLayout(
                        actionButton("area_modify", "Gebiet ändern"),
                        p(""))
                    o[[length(o) + 1]] <- p("")
                    
                }
            } else {
                if (area(sp.extent) > 25000000) {
                    o[[length(o) + 1]] <- 
                        p(paste0("Die gewählte Fläche ist ", 
                                 round(area(sp.extent) / 10000, 2), " ha groß ", 
                                 "(", crs_comp_reactive(), ") und damit größer", 
                                 " als die maximal erlaubten 2500 ha. Bitte re", 
                                 "duzieren Sie die Flächengröße:"))
                    o[[length(o) + 1]] <- p(paste0("x-Ausdehnung: ", 
                                                   extent_etrs@xmax - 
                                                       extent_etrs@xmin, " m"))
                    o[[length(o) + 1]] <- splitLayout(
                        numericInput("xmin", "xmin", extent_etrs@xmin),
                        numericInput("xmax", "xmax", extent_etrs@xmax))
                    o[[length(o) + 1]] <- p(paste0("y-Ausdehnung: ", 
                                                   extent_etrs@ymax - 
                                                       extent_etrs@ymin, " m"))
                    o[[length(o) + 1]] <- splitLayout(
                        numericInput("ymin", "ymin", extent_etrs@ymin),
                        numericInput("ymax", "ymax", extent_etrs@ymax))
                    o[[length(o) + 1]] <- p("")
                    o[[length(o) + 1]] <- splitLayout(
                        actionButton("area_modify", "Gebiet ändern"),
                        p(""))
                    o[[length(o) + 1]] <- p("")
                    
                } else if (area(sp.extent) <= 25000000) {
                    o[[length(o) + 1]] <- 
                        p(paste0("Die gewählte Fläche ist ", 
                                 round(area(sp.extent) / 10000, 2), " ha groß ",
                                 "(", crs_comp_reactive(), ")."))
                    o[[length(o) + 1]] <- p(paste0("x-Ausdehnung: ", 
                                                   extent_etrs@xmax - 
                                                       extent_etrs@xmin, " m"))
                    o[[length(o) + 1]] <- splitLayout(
                        numericInput("xmin", "xmin", extent_etrs@xmin),
                        numericInput("xmax", "xmax", extent_etrs@xmax))
                    o[[length(o) + 1]] <- p(paste0("y-Ausdehnung: ", 
                                                   extent_etrs@ymax - 
                                                       extent_etrs@ymin, " m"))
                    o[[length(o) + 1]] <- splitLayout(
                        numericInput("ymin", "ymin", extent_etrs@ymin),
                        numericInput("ymax", "ymax", extent_etrs@ymax))
                    o[[length(o) + 1]] <- p("")
                    
                    o[[length(o) + 1]] <- splitLayout(
                        actionButton("area_modify", "Gebiet ändern"),
                        actionButton("area_confirm", "Gebiet bestätigen"))
                    o[[length(o) + 1]] <- p("")
                }
            }
        }
        
        output$area <- renderUI({o})
        
    })
    
    # confirm
    observeEvent(input$area_confirm, {
        
        # modify rV
        res$extent <- c(input$xmin, input$xmax, input$ymin, input$ymax)
        res$crs <- as.character(crs_reactive())
        
        # modify map
        l <- leafletProxy("map")
        
        if (!is.null(df.coor_area_reactive())) {
            l %>% removeDrawToolbar(clearFeatures = TRUE)
            l %>% removeShape(layerId = c("area_tmp", "area"))
            l %>% addPolygons(lng = df.coor_area_reactive()$lng, 
                              lat = df.coor_area_reactive()$lat, 
                              color = "black", weight = 2, 
                              fillColor = "lightblue", 
                              fillOpacity = 0.7, 
                              layerId = "area")
            e <- extent(
                SpatialPolygons(
                    list(Polygons(
                        list(Polygon(df.coor_area_reactive())), ID = 1)), 
                    proj4string = crs))
            l %>% fitBounds(lng1 = e@xmin - (e@xmax - e@xmin) / 3,
                            lat1 = e@ymin - (e@ymax - e@ymin) / 3,
                            lng2 = e@xmax + (e@xmax - e@xmin) / 3,
                            lat2 = e@ymax + (e@ymax - e@ymin) / 3)
        }
        
        # modify UI
        disable("river")
        disable("from_to")
        
        # capture ETRS extent
        extent_etrs <- extent(res$extent)
        sp.extent_etrs <- as(extent_etrs, "SpatialPolygons")
        crs(sp.extent_etrs) <- crs_reactive()
        
        o <- tagList(h4("Berechnungsgebiet"))
        o[[length(o) + 1]] <- 
            p(paste0("Fläche: ", round(area(sp.extent_etrs) / 10000, 2), " ha ",
                     "(", extent_etrs@xmax - extent_etrs@xmin, " x ",
                     extent_etrs@ymax - extent_etrs@ymin, " m)"))
        o[[length(o) + 1]] <- splitLayout(
            disabled(numericInput("xmin", "xmin", extent_etrs@xmin)),
            disabled(numericInput("xmax", "xmax", extent_etrs@xmax)))
        o[[length(o) + 1]] <- splitLayout(
            disabled(numericInput("ymin", "ymin", extent_etrs@ymin)),
            disabled(numericInput("ymax", "ymax", extent_etrs@ymax)))
        o[[length(o) + 1]] <- p("")
        
        output$area <- renderUI({o})
        
    })
    
    # Deleted features
    observeEvent(input$map_draw_deleted_features, {
        l <- leafletProxy("map")
        l %>% removeShape(layerId = "area")
        l %>% removeShape(layerId = "area_tmp")
    })
    
    ############################################################################
    #####
    # respond to menu item 3: area
    ###
    # enable menu 4
    observeEvent(res$crs, {
        if (!is.na(res$crs)) {
            output$seq <- renderUI({
                tagList(h4("Berechnungszeitraum"),
                        
                        dateRangeInput(
                            inputId = "seq_from_to",
                            label = NULL,
                            start = "2017-01-01",
                            end = "2017-12-31",
                            min = "1990-01-01",
                            max = as.character(Sys.Date() - 1),
                            format = "dd.mm.yyyy",
                            startview = "year",
                            weekstart = 1,
                            language = "de",
                            separator = " bis "
                        ),
                        
                        splitLayout(p(""), 
                                    actionButton("seq_confirm", 
                                                 "Zeiten bestätigen"))
                )
            })
        }
    })
    
    #####
    # respond to menu item 4: seq
    ###
    # responsive datasets
    ##
    # uppermost gauging station in area or next upstream to area
    gs_reactive <- reactive({
        # upper most computation-relevant gauging_station
        if (!is.na(res$crs)) {
            
            # convert extent to SpatialPolygons.*
            extent_etrs <- extent(res$extent)
            sp.area_etrs_valid <- as(extent_etrs, 'SpatialPolygons')
            crs(sp.area_etrs_valid) <- crs_reactive()
            sp.area_valid <- spTransform(sp.area_etrs_valid, crs)
            
            # get gauging stations in sp.area_valid
            l.gsd <- over(sp.area_valid, spdf.gsd, returnList = TRUE)
            
            if (nrow(l.gsd[[1]]) < 1) {
                l.station <- over(sp.area_valid, spdf.station, 
                                  returnList = TRUE)
                if (nrow(l.station[[1]]) < 1) {
                    return(NULL)
                } else {
                    min_station <- min(l.station[[1]]$station)
                    ids <- numeric()
                    id_up <- which(spdf.gsd@data$river == toupper(res$river) &
                                       spdf.gsd@data$km_qps <= min_station)
                    if (length(id_up) > 0) {ids <- append(ids, max(id_up))}
                    id_do <- which(spdf.gsd@data$river == toupper(res$river) &
                                       spdf.gsd@data$km_qps > min_station)
                    if (length(id_do) > 0) {ids <- append(ids, min(id_do))}
                    spdf.gsd_sel <- spdf.gsd[ids, ]
                    
                    id <- which.min(abs(spdf.gsd_sel@data$km_qps - min_station))
                    
                    if (length(id) == 1) {
                        return(spdf.gsd_sel@data$gauging_station[id])
                    } else {
                        return(NULL)
                    }
                }
            } else {
                spdf.gsd_sel <- l.gsd[[1]]
                id <- which(spdf.gsd@data$km_qps == 
                                min(spdf.gsd_sel@data$km_qps))
                return(spdf.gsd@data$gauging_station[id])
            }
        }
    })
    
    # pnp of the uppermost gauging station
    pnp_reactive <- reactive({
        if (is.null(gs_reactive())) {
            return(NULL)
        } else {
            spdf.gsd@data$pnp[which(spdf.gsd@data$gauging_station == 
                                        gs_reactive())]
        }
    })
    
    # gauging data of the uppermost gauging station for seq
    df.gd_reactive <- reactive({
        if (is.null(gs_reactive())) {
            df.gd[FALSE, ]
        } else {
            df.gd[which(df.gd$gauging_station == gs_reactive() &
                        df.gd$date >= as.Date(res$seq_from_to[1]) &
                        df.gd$date <= as.Date(res$seq_from_to[2])), ]
        }
    })
    
    ###
    # observe changes of input$seq_confirm and create menu items 5 + 6: 
    # plot of gauging data and email form
    ##
    observeEvent(input$seq_confirm, {
        
        res$seq_from_to <- as.character(input$seq_from_to)
        
        output$seq <- renderUI({
            tagList(
                h4("Berechnungszeitraum"),
                disabled(
                    dateRangeInput(
                        inputId = "seq_from_to",
                        label = NULL,
                        start = res$seq_from_to[1],
                        end = res$seq_from_to[2],
                        min = "1990-01-01",
                        max = as.character(Sys.Date() - 1),
                        format = "dd.mm.yyyy",
                        startview = "year",
                        weekstart = 1,
                        language = "de",
                        separator = " bis "
                    )
                )
            )
        })
        
        if (nrow(df.gd_reactive()) > 0) {
            
            res$df.gd <- data.frame(x = df.gd_reactive()$date,
                                    y = (df.gd_reactive()$w / 100) + 
                                            pnp_reactive())
            res$gs <- gs_reactive()
            output$seq_gs <- renderPlot({
                date_min <- min(df.gd_reactive()$date)
                date_max <- max(df.gd_reactive()$date)
                par(oma = c(1, 1, 1, 1), mar = c(1, 4, 1.5, 1), cex = 0.8)
                plot(x = res$df.gd$x, y = res$df.gd$y, xlab = "",
                     ylab = "m über NHN (DHHN 92)", xaxt = "n", type = "l",
                     col = "darkblue", main = paste0("PEGEL: ", res$gs))
                axis.Date(side = 1, at = c(date_min, date_max), 
                          labels = c(strftime(date_min, "%d.%m.%Y"),
                                     strftime(date_max, "%d.%m.%Y")))
                box()
            }, width = 300, height = 150)
        }
        
        output$email <- renderUI({
            tagList(h4("Email"),
                    
                    textInput(
                        inputId = "email",
                        label = NULL
                    ),
                    
                    splitLayout(p(""), 
                                actionButton("email_confirm", 
                                             "Email bestätigen"))
            )
        })
    })
    
    #####
    # respond to menu item 6: email
    ###
    # observe changes of input$email_confirm and create menu item 7: submit
    ##
    observeEvent(input$email_confirm, {
        
        if (isValidEmail(input$email)) {
            
            res$email <- input$email
            
            output$email <- renderUI({
                tagList(
                    h4("Email"),
                    disabled(
                        textInput(
                            inputId = "email",
                            label = NULL,
                            value = res$email
                        )
                    )
                )
            })
            
            output$submit <- renderUI({
                tagList(
                    h3("Berechnung"),
                    splitLayout(p(""), 
                                actionButton("submit_confirm", "Starten"))
                )
            })
        } else {
            
            output$email <- renderUI({
                tagList(
                    h4("Email"),
                    p(paste0("Die angegebene Emailadresse ist ungültig. Bi",
                             "tte passen Sie die Emailadresse an und versu",
                             "chen es erneut!")),
                    textInput(
                        inputId = "email",
                        label = NULL,
                        value = input$email
                    ),
                    splitLayout(p(""), 
                                actionButton("email_confirm", 
                                             "Email bestätigen"))
                )
            })
        }
    })
    
    #####
    # respond to menu item 7: submit
    ###
    # 
    observeEvent(input$submit_confirm, {
        
        # create random string for out and log files
        if (is.na(res$random)) {
            
            # send a first email message
            system(paste0("mail -s '[shiny-flood3]: Berechnung gestartet' ", 
                          res$email, " < scripts/01_mail"))
            
            # send.mail(from = "arnd.weber@bafg.de",
            #           to = "arnd.weber@bafg.de", #c(res$email),
            #           subject = "Shiny-Service: flood3()",
            #           body = paste0("Sehr geerhte Nutzerin, sehr geehrter Nutz",
            #                         "er,\n\nSoeben wurde ihre Berechnung der Ü",
            #                         "berflutungsdauer wurde gestartet. Nach Ab",
            #                         "schluss der Berechnungen erhalten Sie ern",
            #                         "eut eine Email mit einem Link zum Downloa",
            #                         "d des Berechnungsproduktes.\n\nMit freund",
            #                         "lichen Grüßen\nIm Auftrag\nIhre BfG"),
            #           smtp = list(host.name = "host", port = 465,
            #                       user.name = "user",
            #                       passwd = "password", ssl = TRUE),
            #           authenticate = TRUE, send = TRUE, encoding = "utf-8")
            
            random <- randomString(length = 20)
            while (file.exists(paste0("in_process/", random, ".RData")) |
                   dir.exists(paste0("processed/", random))) {
                random <- randomString(length = 20)
            }
            res$random <- random
            
            out <- paste0("in_process/", random, ".RData")
            log <- paste0("in_process/", random, ".log")
            
            # save reactive res to out
            l.res <- reactiveValuesToList(res)
            save(l.res, file = out)
            
            # trigger processing
            system(paste0("nohup Rscript scripts/processing.R ", 
                          out, " > ", log, " 2>&1 &"))
        }
        
        # message
        if (file.exists(paste0("www/downloads/", res$random, ".zip"))) {
            if (res$restored) {
                output$submit <- renderUI({
                    tagList(
                        h3("Berechnung beendet"),
                        p(paste0("Unter folgendem Link kann der Rasterdatensat",
                                 "z der Überflutungsdauer heruntergeladen werd",
                                 "en:")),
                        a(id = "download",
                          class = paste0("btn btn-default shiny-download-link ",
                                         "shiny-bound-output"),
                          href = paste0("http://", 
                                        session$clientData$url_hostname, 
                                        session$clientData$url_pathname, 
                                        "downloads/", res$random, ".zip"), 
                          icon = icon(name = "fa-download"), 
                          "Download"),
                        p(""),
                        splitLayout(
                            p(""),
                            actionButton("reset", "Neue Berechnung")
                        )
                    )
                })
            } else {
                output$submit <- renderUI({
                    tagList(
                        h3("Berechnung beendet"),
                        p(paste0("Unter folgendem Link kann der Rasterdatensat",
                                 "z der Überflutungsdauer heruntergeladen werd",
                                 "en:")),
                        a(id = "download",
                          class = paste0("btn btn-default shiny-download-link ",
                                         "shiny-bound-output"),
                          href = paste0("http://", 
                                        session$clientData$url_hostname, 
                                        session$clientData$url_pathname, 
                                        "downloads/", res$random, ".zip"), 
                          icon = icon(name = "fa-download"), 
                          "Download"),
                        p(""),
                        splitLayout(
                            bookmarkButton(label = "Bookmark", 
                                           title = paste0("Setze ein Bookmark ",
                                                          "des Anwendungszusta",
                                                          "nds"), 
                                           id = "bookmark"),
                            actionButton("reset", "Neue Berechnung")
                        )
                    )
                })
            }
        } else {
            if (file.exists(paste0("processed/", res$random, "/", res$random, 
                                   ".RData"))) {
                output$submit <- renderUI({
                    tagList(
                        h3("Berechnung beendet"),
                        p(paste0("Die Berechung wurde vor mehr als sieben Tage",
                                 "n abgeschlossen und die Produkte wurden zwis",
                                 "chenzeitig gelöscht.")),
                        p(""),
                        splitLayout(
                            p(""),
                            actionButton("reset", "Neue Berechnung")
                        )
                    )
                })
            } else {
                if (res$restored) {
                    output$submit <- renderUI({
                        tagList(
                            h3("Berechnung gestartet"),
                            p(paste0("Nach Abschluss dieser erhalten Sie eine ",
                                     "weitere Email mit einem Downloadlink des",
                                     " produzierten Rasterdatensatzes.")),
                            p(""),
                            splitLayout(
                                p(""),
                                actionButton("reset", "Neue Berechnung")
                            )
                        )
                    })
                } else {
                    output$submit <- renderUI({
                        tagList(
                            h3("Berechnung gestartet"),
                            p(paste0("Nach Abschluss dieser erhalten Sie eine ",
                                     "weitere Email mit einem Downloadlink des",
                                     " produzierten Rasterdatensatzes.")),
                            p(""),
                            splitLayout(
                                bookmarkButton(label = "Bookmark", 
                                               title = paste0("Setze ein Bookm",
                                                              "ark des Anwendu",
                                                              "ngszustands"), 
                                               id = "bookmark"),
                                actionButton("reset", "Neue Berechnung")
                            )
                        )
                    })
                }
            }
        }
        
    })
    
    #####
    # respond to menu item 8: bookmark &| reset &| download link
    ###
    # bookmark
    setBookmarkExclude(c("bookmark"))
    observeEvent(input$bookmark, {
        session$doBookmark()
    })
    
    # onBookmark
    onBookmark(function(state) {
        state$values$res_river <- res$river
        state$values$res_from_to <- res$from_to
        state$values$res_river_from_to <- res$river_from_to
        state$values$res_extent <- res$extent
        state$values$res_crs <- res$crs
        state$values$res_seq_from_to <- res$seq_from_to
        state$values$res_df.gs <- res$df.gs
        state$values$res_gs <- res$gs
        state$values$res_email <- res$email
        state$values$res_random <- res$random
    })
    
    # onRestore
    onRestore(function(state) {
        res$river <- state$values$res_river
        res$from_to <- state$values$res_from_to
        res$river_from_to <- state$values$res_river_from_to
        res$extent <- state$values$res_extent
        res$crs <- state$values$res_crs
        res$seq_from_to <- state$values$res_seq_from_to
        res$df.gs <- state$values$res_df.gs
        res$gs <- state$values$res_gs
        res$email <- state$values$res_email
        res$random <- state$values$res_random
        res$restored <- TRUE
    })
    
    # onRestored
    onRestored(function(state) {
        l <- leafletProxy("map")
        
        # define plotting order
        l %>% addMapPane("af", zIndex = 410)
        l %>% addMapPane("ufd", zIndex = 420)
        l %>% addMapPane("area", zIndex = 430)
        l %>% addMapPane("gs", zIndex = 440)
        
        # add active floodplain polygons
        if (res$river == "Elbe") {
            l %>% removeShape(layerId = c("afr"))
            l %>% addPolygons(lng = df.coor.afe$lon, lat = df.coor.afe$lat,
                              label = "Elbe", color = "blue", weight = 2,
                              fill = TRUE, fillColor = "lightblue", 
                              fillOpacity = 0.6, layerId = "afe",
                              options = pathOptions(pane = "af"))
        } else {
            l %>% removeShape(layerId = c("afe"))
            l %>% addPolygons(lng = df.coor.afr$lon, lat = df.coor.afr$lat,
                              label = "Rhein", color = "blue", weight = 2,
                              fill = TRUE, fillColor = "lightblue", 
                              fillOpacity = 0.6, layerId = "afr",
                              options = pathOptions(pane = "af"))
        }
        
        # add gauging stations
        l %>% addCircles(lng = ~longitude, 
                         lat = ~latitude, 
                         label = htmlEscape(~gauging_station), 
                         popup = htmlEscape(~gauging_station), 
                         group = "gs", color = "black", opacity = 1, 
                         fillColor = "yellow", fillOpacity = 1, 
                         data = spdf.gsd_reactive(),
                         options = pathOptions(pane = "gs"))
        
        # add area
        l %>% addPolygons(lng = df.coor_area_reactive()$lng, 
                          lat = df.coor_area_reactive()$lat, 
                          color = "black", weight = 2, 
                          fillColor = "lightblue", 
                          fillOpacity = 0.7, 
                          layerId = "area",
                          options = pathOptions(pane = "area"))
        
        # display the raster product, if it exists
        geotiff <- paste0("processed/", res$random, "/", res$river, "_",
                          paste0(res$extent, collapse = "-"), "_",
                          paste0(res$seq_from_to, collapse = "-"), "_wgs84.tif")
        if (file.exists(geotiff)) {
            r <- raster(geotiff, crs = crs)
            ufd_col <- colorRampPalette(c("red", "yellow", "green", "darkblue"))
            pal <- colorNumeric(palette = ufd_col(10),
                                domain = values(r),
                                na.color = "transparent")
            
            l %>% removeShape(layerId = c("area", "afe", "afr"))
            if (res$river == "Elbe") {
                l %>% addPolygons(lng = df.coor.afe$lon, lat = df.coor.afe$lat,
                                  label = "Elbe", color = "blue", weight = 2,
                                  fill = FALSE, layerId = "afe",
                                  options = pathOptions(pane = "af"))
            } else {
                l %>% addPolygons(lng = df.coor.afr$lon, lat = df.coor.afr$lat,
                                  label = "Rhein", color = "blue", weight = 2,
                                  fill = FALSE, layerId = "afr",
                                  options = pathOptions(pane = "af"))
            }
            l %>% addPolygons(lng = df.coor_area_reactive()$lng,
                              lat = df.coor_area_reactive()$lat,
                              color = "black", weight = 2, layerId = "area",
                              fill = FALSE,
                              options = pathOptions(pane = "area"))
            l %>% addRasterImage(x = r, colors = pal, opacity = 1, 
                                 project = FALSE)
            l %>% addLegend("bottomleft", title = "Überflutungsdauer (d)",
                            pal = pal, values = values(r), opacity = 1)
        }
        
        e <- extent(
            SpatialPolygons(
                list(Polygons(
                    list(Polygon(df.coor_area_reactive())), ID = 1)), 
                proj4string = crs))
        l %>% fitBounds(lng1 = e@xmin - (e@xmax - e@xmin) / 3,
                        lat1 = e@ymin - (e@ymax - e@ymin) / 3,
                        lng2 = e@xmax + (e@xmax - e@xmin) / 3,
                        lat2 = e@ymax + (e@ymax - e@ymin) / 3)
        
    })
    
    ###
    # reset
    observeEvent(input$reset, {
        
        # reset res
        res$river <- NA_character_
        res$from_to <- c(NA_real_, NA_real_)
        res$river_from_to <- c(NA_real_, NA_real_)
        res$extent <- c(NA_real_, NA_real_, NA_real_, NA_real_)
        res$crs <- NA_character_
        res$seq_from_to <- c(NA_character_, NA_character_)
        res$df.gs <- data.frame()
        res$gs <- NA_character_
        res$email <- NA_character_
        res$random <- NA_character_
        res$restored <- FALSE
        
        # reload the base URL via Javascript
        shinyjs::runjs(paste0('window.location.href = "http://', 
                              session$clientData$url_hostname, 
                              session$clientData$url_pathname, '";'))
        
    })
    
}
