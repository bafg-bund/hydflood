library(shiny, lib.loc = lib)
library(shinyjs, lib.loc = lib)
library(leaflet, lib.loc = lib)
library(leaflet.extras, lib.loc = lib)
library(sp, lib.loc = lib)
library(raster, lib.loc = lib)
library(rgdal, lib.loc = lib)
library(rgeos, lib.loc = lib)
library(hyd1d, lib.loc = lib)
library(hydflood3, lib.loc = lib)
library(htmltools, lib.loc = lib)
library(sendmailR, lib.loc = lib)

function(input, output, session) {
    
    # leaflet background map
    output$map <- renderLeaflet({
        leaflet() %>% addTiles() %>% 
            setMaxBounds(lng1 = min(c(df.coor.afe$lon, df.coor.afr$lon)), 
                         lat1 = min(c(df.coor.afe$lat, df.coor.afr$lat)), 
                         lng2 = max(c(df.coor.afe$lon, df.coor.afr$lon)), 
                         lat2 = max(c(df.coor.afe$lat, df.coor.afr$lat))) %>%
            fitBounds(lng1 = min(c(df.coor.afe$lon, df.coor.afr$lon)),
                      lat1 = min(c(df.coor.afe$lat, df.coor.afr$lat)),
                      lng2 = max(c(df.coor.afe$lon, df.coor.afr$lon)),
                      lat2 = max(c(df.coor.afe$lat, df.coor.afr$lat)))
    })
    
    # reactive values to trigger menu items
    menu_elements <- reactiveValues(
        river = 0, 
        from_to = 0, 
        area = 0, 
        seq = 0, 
        email = 0, 
        submit = 0
    )
    
    # reactive values to trigger the computation
    res_elements <- reactiveValues(
        river = NA_character_,
        crs = NA_character_,
        extent = c(NA_real_, NA_real_, NA_real_, NA_real_), 
        seq_from_to = c(NA_character_, NA_character_), 
        email = NA_character_ 
    )
    
    #####
    # create menu item 1
    observe({
        if (is.null(input$river) | menu_elements$river == 0) {
            output$river <- renderUI(
                selectInput(
                    inputId  = "river",
                    label    = "Fluss:",
                    choices  = rivers,
                    selected = "Bitte wählen Sie!"
                )
            )
        } else {
            if (menu_elements$area == 0) {
                output$river <- renderUI(
                    selectInput(
                        inputId  = "river",
                        label    = "Fluss:",
                        choices  = rivers,
                        selected = res_elements$river
                    )
                )
            } else {
                output$river <- renderUI(
                    selectInput(
                        inputId  = "river",
                        label    = "Fluss:",
                        choices  = rivers,
                        selected = res_elements$river
                    )
                )
                disable("river")
            }
        }
    }, priority = 10)
    
    #####
    # respond to menu item 1: river
    ###
    # responsive datasets
    ##
    # responsive df.from_to
    df.from_to_reactive <- reactive({
        if (is.null(input$river)) {
            return(df.from_to[FALSE, ])
        } else {
            return(df.from_to[which(df.from_to$river == input$river), ])
        }
    })
    
    # responsive crs
    crs_reactive <- reactive({
        if (is.null(input$river)) {
            return(NULL)
        } else if (input$river == "Elbe") {
            return(CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"))
        } else if (input$river == "Rhein") {
            return(CRS("+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs"))
        } else {
            return(NULL)
        }
    })
    
    crs_comp_reactive <- reactive({
        if (is.null(input$river)) {
            return(NULL)
        } else if (input$river != "Bitte wählen Sie!") {
            return(df.crs_comp$crs[which(df.crs_comp$river == input$river)])
        } else {
            return(NULL)
        }
    })
    
    # responsive active floodplains in WGS 1984
    spdf.af_reactive <- reactive({
        if (is.null(input$river)) {
            return(NULL)
        } else if (input$river == "Elbe") {
            return(spdf.afe)
        } else if (input$river == "Rhein") {
            return(spdf.afr)
        } else {
            return(NULL)
        }
    })
    
    # responsive active floodplains in ETRS
    spdf.af_etrs_reactive <- reactive({
        if (is.null(input$river)) {
            return(NULL)
        } else if (input$river == "Elbe") {
            return(spdf.active_floodplain_elbe)
        } else if (input$river == "Rhein") {
            return(spdf.active_floodplain_rhein)
        } else {
            return(NULL)
        }
    })
    
    # responsive spdf.gs
    spdf.gs_reactive <- reactive({
        if (is.null(input$river)) {
            return(NULL)
        } else if (input$river != "Bitte wählen Sie!") {
            return(spdf.gs[which(spdf.gs$river == toupper(input$river)), ])
        } else {
            return(NULL)
        }
    })
    
    ###
    # observe changes of input$river
    ##
    # background and rV: menu_elements
    observe({
        if (!is.null(input$river)){
            if (input$river == 'Bitte wählen Sie!') {
                # base map
                l <- leafletProxy("map") %>%
                    fitBounds(lng1 = min(c(df.coor.afe$lon, df.coor.afr$lon)),
                              lat1 = min(c(df.coor.afe$lat, df.coor.afr$lat)),
                              lng2 = max(c(df.coor.afe$lon, df.coor.afr$lon)),
                              lat2 = max(c(df.coor.afe$lat, df.coor.afr$lat)))
                
                # rV menu elements
                menu_elements$river <- 0
                menu_elements$from_to <- 0
                menu_elements$area <- 0
                menu_elements$seq <- 0
                menu_elements$email <- 0
                menu_elements$submit <- 0
                
                enable("river")
                enable("from_to")
                
            } else {
                menu_elements$river <- 1
                menu_elements$from_to <- 0
                menu_elements$area <- 0
                menu_elements$seq <- 0
                menu_elements$email <- 0
                menu_elements$submit <- 0
            }
        }
    }, priority = 10)
    
    ##
    # active floodplain polygons
    observe({
        
        l <- leafletProxy("map")
        
        if (is.null(input$river)) {
            l %>% addPolygons(lng = df.coor.afe$lon, lat = df.coor.afe$lat,
                              label = "Elbe", color = "blue", weight = 2,
                              fill = TRUE, fillColor = "lightblue", 
                              fillOpacity = 0.6, layerId = "afe")
            l %>% addPolygons(lng = df.coor.afr$lon, lat = df.coor.afr$lat,
                              label = "Rhein", color = "blue", weight = 2,
                              fill = TRUE, fillColor = "lightblue", 
                              fillOpacity = 0.6, layerId = "afr")
        } else {
            if (input$river == "Elbe") {
                l %>% removeShape(layerId = c("afr"))
                l %>% addPolygons(lng = df.coor.afe$lon, lat = df.coor.afe$lat,
                                  label = "Elbe", color = "blue", weight = 2,
                                  fill = TRUE, fillColor = "lightblue", 
                                  fillOpacity = 0.6, layerId = "afe")
            } else if (input$river == "Rhein") {
                l %>% addPolygons(lng = df.coor.afr$lon, lat = df.coor.afr$lat,
                                  label = "Rhein", color = "blue", weight = 2,
                                  fill = TRUE, fillColor = "lightblue", 
                                  fillOpacity = 0.6, layerId = "afr")
            } else {
                l %>% addPolygons(lng = df.coor.afe$lon, lat = df.coor.afe$lat,
                                  label = "Elbe", color = "blue", weight = 2,
                                  fill = TRUE, fillColor = "lightblue", 
                                  fillOpacity = 0.6, layerId = "afe")
                l %>% addPolygons(lng = df.coor.afr$lon, lat = df.coor.afr$lat,
                                  label = "Rhein", color = "blue", weight = 2,
                                  fill = TRUE, fillColor = "lightblue", 
                                  fillOpacity = 0.6, layerId = "afr")
            }
        }
    })
    
    ##
    # gauging stations
    observeEvent(spdf.gs_reactive(), {
        
        l <- leafletProxy("map")
        l %>% clearGroup(group = "gs")
        
        if (!is.null(spdf.gs_reactive())){
            l %>% addCircles(lng = ~longitude, 
                             lat = ~latitude, 
                             label = htmlEscape(~gauging_station), 
                             popup = htmlEscape(~gauging_station), 
                             color = "black", opacity = 1, 
                             fillColor = "yellow", fillOpacity = 1, 
                             group = "gs",
                             data = spdf.gs_reactive())
        }
    })
    
    #####
    # create or update menu item 2
    observe({
        if (menu_elements$river == 0 & menu_elements$from_to == 0) {
            output$from_to <- renderUI("")
        } else if (menu_elements$river == 1 & menu_elements$from_to == 0) {
            isolate(
                output$from_to <- renderUI({
                    sliderInput(
                        inputId = "from_to",
                        label   = "Abschnitt (von km - bis km):",
                        min     = df.from_to_reactive()$from_val,
                        max     = df.from_to_reactive()$to_val,
                        value   = c(df.from_to_reactive()$from, 
                                    df.from_to_reactive()$to),
                        step    = 0.1
                    )
                })
            )
        } else {
            if (menu_elements$river == 1 & menu_elements$from_to == 1) {
                output$from_to <- renderUI({
                    sliderInput(session,
                                inputId = "from_to",
                                label   = "Kilometer (von - bis):",
                                min     = df.from_to_reactive()$from_val,
                                max     = df.from_to_reactive()$to_val,
                                value   = c(res_elements$from_to[1],
                                            res_elements$from_to[2]),
                                step    = 0.1)
                })
            }
            
            if (menu_elements$area == 1) {
                disable("from_to")
            }
        }
    })
    
    ##
    # # update the sliderInput for menu 2
    # observe({
    #     updateSliderInput(session,
    #                       inputId = "from_to",
    #                       label   = "Kilometer (von - bis):",
    #                       min     = df.from_to_reactive()$from_val,
    #                       max     = df.from_to_reactive()$to_val,
    #                       value   = c(df.from_to_reactive()$from,
    #                                   df.from_to_reactive()$to),
    #                       step    = 0.1
    #     )
    # })
    
    #####
    # respond to menu item 2: from_to
    ###
    # responsive datasets
    ##
    # responsive spdf.station
    spdf.station_reactive <- reactive({
        if (is.null(input$river)) {
            return(spdf.station[FALSE, ])
        } else if (!is.null(input$river) & all(is.na(input$from_to))) { 
            return(spdf.station[which(spdf.station$river == input$river), ])
        } else if (!is.null(input$river) & !any(is.na(input$from_to))) {
            return(spdf.station[
                       which(spdf.station$river == input$river &
                             spdf.station$station >= input$from_to[1] &
                             spdf.station$station <= input$from_to[2]), ])
        } else {
            return(spdf.station)
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
    
    # modify rV: menu_elements
    observeEvent(input$from_to, {
        browser()
        if (input$from_to[1] != df.from_to_reactive()$from_val) {
            menu_elements$from_to <- 1
            menu_elements$area <- 0
            menu_elements$seq <- 0
            menu_elements$email <- 0
            menu_elements$submit <- 0
        } else if (input$from_to[2] != df.from_to_reactive()$to_val) {
            menu_elements$from_to <- 1
            menu_elements$area <- 0
            menu_elements$seq <- 0
            menu_elements$email <- 0
            menu_elements$submit <- 0
        } else {
            menu_elements$from_to <- 0
            menu_elements$area <- 0
            menu_elements$seq <- 0
            menu_elements$email <- 0
            menu_elements$submit <- 0
        }
    }, ignoreInit = TRUE)
    
    # reactive boolean, wether drawing toolbar should be enabled
    draw_reactive <- reactive({
        if(menu_elements$river == 1   &
           menu_elements$from_to == 1 &
           menu_elements$area == 0    &
           menu_elements$seq == 0     &
           menu_elements$email == 0   &
           menu_elements$submit == 0) {
            return(TRUE)
        } else {
            return(FALSE)
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
                                      color = "black", weight = 5,
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
                              color = "black", weight = 5,
                              fillColor = "lightblue", 
                              fillOpacity = 0.7,
                              layerId = "area_temp")
            e <- extent(
                SpatialPolygons(
                    list(Polygons(
                            list(Polygon(df.coor_area_reactive())), ID = 1)), 
                    proj4string = crs_reactive()))
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
        menu_elements$area <- 1
        res_elements$river <- input$river
        res_elements$crs <- as.character(crs_reactive())
        res_elements$extent <- c(input$xmin, input$xmax, input$ymin, input$ymax)
        
        # modify map
        l <- leafletProxy("map")
        
        if (!is.null(df.coor_area_reactive())) {
            l %>% removeDrawToolbar(clearFeatures = TRUE)
            l %>% removeShape(layerId = c("area_tmp", "area"))
            l %>% addPolygons(lng = df.coor_area_reactive()$lng, 
                              lat = df.coor_area_reactive()$lat,
                              color = "black", weight = 5,
                              fillColor = "lightblue", 
                              fillOpacity = 0.7,
                              layerId = "area")
            e <- extent(
                SpatialPolygons(
                    list(Polygons(
                        list(Polygon(df.coor_area_reactive())), ID = 1)), 
                    proj4string = crs_reactive()))
            l %>% fitBounds(lng1 = e@xmin - (e@xmax - e@xmin) / 3,
                            lat1 = e@ymin - (e@ymax - e@ymin) / 3,
                            lng2 = e@xmax + (e@xmax - e@xmin) / 3,
                            lat2 = e@ymax + (e@ymax - e@ymin) / 3)
        }
        
        # modify UI
        disable("river")
        disable("from_to")
        #removeUI("from_to")
        
        # capture ETRS extent
        extent_etrs <- extent(input$xmin, input$xmax, input$ymin, input$ymax)
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
    observeEvent(menu_elements$area, {
        if (menu_elements$area ==  1) {
            output$seq <- renderUI({
                tagList(h4("Berechnungszeitraum"),
                        
                        dateRangeInput(
                            inputId = "seq",
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
        if (! is.null(menu_elements$area)) {
            if (menu_elements$area == 1) {
                # convert extent to SpatialPolygons.*
                extent_etrs <- extent(res_elements$extent)
                sp.area_etrs_valid <- as(extent_etrs, 'SpatialPolygons')
                crs(sp.area_etrs_valid) <- crs_reactive()
                sp.area_valid <- spTransform(sp.area_etrs_valid, crs)
                
                # get gauging stations in sp.area_valid
                l.gs <- over(sp.area_valid, spdf.gs, returnList = TRUE)
                
                if (nrow(l.gs[[1]]) < 1) {
                    l.station <- over(sp.area_valid, spdf.station, 
                                      returnList = TRUE)
                    if (nrow(l.station[[1]]) < 1) {
                        return(NULL)
                    } else {
                        min_station <- min(l.station[[1]]$station)
                        ids <- numeric()
                        id_up <- which(spdf.gs$river == toupper(input$river) &
                                       spdf.gs$km_qps <= min_station)
                        if (length(id_up) > 0) {ids <- append(ids, max(id_up))}
                        id_do <- which(spdf.gs$river == toupper(input$river) &
                                       spdf.gs$km_qps > min_station)
                        if (length(id_do) > 0) {ids <- append(ids, id_do)}
                        spdf.gs_sel <- spdf.gs[ids, ]
                        
                        id <- which.min(abs(spdf.gs_sel$km_qps - min_station))
                        
                        if (length(id) == 1) {
                            return(spdf.gs_sel$gauging_station[id])
                        } else {
                            return(NULL)
                        }
                    }
                } else {
                    df.gs <- l.gs[[1]]
                    id <- which(spdf.gs$km_qps == min(spdf.gs$km_qps))
                    return(df.gs$gauging_station[id])
                }
            }
        }
    })
    
    # pnp of the uppermost gauging station
    pnp_reactive <- reactive({
        if (is.null(gs_reactive())) {
            return(NULL)
        } else {
            df.gsd[which(df.gsd$gauging_station == gs_reactive()), "pnp"]
        }
    })
    
    # gauging data of the uppermost gauging station for seq
    df.gd_reactive <- reactive({
        if (is.null(gs_reactive())) {
            df.gd[FALSE, ]
        } else {
            df.gd[which(df.gd$gauging_station == gs_reactive() &
                            df.gd$date >= input$seq[1] &
                            df.gd$date <= input$seq[2]), ]
        }
    })
    
    ###
    # observe changes of input$seq_confirm and create menu items 5 + 6: 
    # plot of gauging data and email form
    ##
    observeEvent(input$seq_confirm, {
        
        menu_elements$seq <- 1
        res_elements$seq_from_to <- as.character(input$seq)
        
        if (all(c(menu_elements$river, menu_elements$from_to, 
                  menu_elements$area, menu_elements$seq) == c(1, 1, 1, 1))) {
            
            output$seq <- renderUI({
                tagList(
                    h4("Berechnungszeitraum"),
                    disabled(
                        dateRangeInput(
                            inputId = "seq",
                            label = NULL,
                            start = input$seq[1],
                            end = input$seq[2],
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
                output$seq_gs <- renderPlot({
                    date_min <- min(df.gd_reactive()$date)
                    date_max <- max(df.gd_reactive()$date)
                    par(oma = c(1, 1, 1, 1), mar = c(1, 4, 1.5, 1), cex = 0.8)
                    plot(x = df.gd_reactive()$date,
                         y = (df.gd_reactive()$w / 100) + pnp_reactive(), xlab = "",
                         ylab = "m über NHN (DHHN 92)", xaxt = "n", type = "l",
                         col = "darkblue", main = paste0("PEGEL: ", gs_reactive()))
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
        }
    })
    
    #####
    # respond to menu item 6: email
    ###
    # observe changes of input$email_confirm and create menu item 7: submit
    ##
    observeEvent(input$email_confirm, {
        
        if (all(c(menu_elements$river, menu_elements$from_to, 
                  menu_elements$area, menu_elements$seq) == c(1, 1, 1, 1))) {
            
            if (isValidEmail(input$email)) {
                
                menu_elements$email <- 1
                res_elements$email <- input$email
                
                output$email <- renderUI({
                    tagList(
                        h4("Email"),
                        disabled(
                            textInput(
                                inputId = "email",
                                label = NULL,
                                value = input$email
                            )
                        )
                    )
                })
                
                output$submit <- renderUI({
                    tagList(
                        h4("Berechnung starten"),
                        actionButton("submit_confirm", "Berechnung starten")
                    )
                })
            } else {
                
                menu_elements$email <- 0
                
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
        }
    })
    
    #####
    # respond to menu item 7: submit
    ###
    # 
    observeEvent(input$submit_confirm, {
        
        if (all(c(menu_elements$river, menu_elements$from_to, 
                  menu_elements$area, menu_elements$seq,
                  menu_elements$email) == c(1, 1, 1, 1, 1))) {
            
            menu_elements$submit <- 1
            
            # send a first email message
            system(paste0("mail -s 'Shiny-Service: flood3()' ", 
                          res_elements$email, " < processing/01_mail"))
            
            # send.mail(from = "arnd.weber@bafg.de",
            #           to = "arnd.weber@bafg.de", #c(res_elements$email),
            #           subject = "Shiny-Service: flood3()",
            #           body = paste0("Sehr gerhte Nutzerin, sehr geehrter Nutze",
            #                         "r,\n\nSoeben wurde ihre Berechnung der Üb",
            #                         "erflutungsdauer wurde gestartet. Nach Abs",
            #                         "chluss der Berechungen erhalten Sie erneu",
            #                         "t eine Email mit einem Link zum Download ",
            #                         "des Berechungsproduktes.\n\nMit freundlic",
            #                         "hen Grüßen\nIm Auftrag\nIhre BfG"),
            #           smtp = list(host.name = "host", port = 465,
            #                       user.name = "user",
            #                       passwd = "password", ssl = TRUE),
            #           authenticate = TRUE, send = TRUE, encoding = "utf-8")
            
            
            # store computation relevant information
            isolate({
                # create random string for out and log files
                random <- randomString(length = 20)
                while (file.exists(paste0("in_process/", random, ".RData")) |
                       dir.exists(paste0("processed/", random))) {
                    random <- randomString(length = 20)
                }
                
                out <- paste0("in_process/", random, ".RData")
                log <- paste0("in_process/", random, ".log")
                
                # save reactive res_elements to out
                res <- reactiveValuesToList(res_elements)
                save(res, file = out)
                
                # trigger processing
                system(paste0("nohup Rscript processing/processing.R ", 
                              out, " > ", log, " 2>&1 &"))
            })
            
            # message
            output$submit <- renderUI({
                tagList(
                    h4("Die Berechnung wurde gestartet!"),
                    p(paste0("Nach Abschluss dieser erhalten Sie eine weitere ",
                             "Email mit einem Downloadlink des produzierten Ra",
                             "sterdatensatzes."))
                )
            })
            
            # reset button
            output$reset <- renderUI({
                tagList(
                    bookmarkButton(label = "Bookmark", 
                                   title = "Setze ein Bookmark des Anwendungszustands", 
                                   id = "bookmark"),
                    actionButton("reset", "Weitere Berechung")
                )
            })
            
        }
    })
    
    #####
    # respond to menu item 8: bookmark $| reset
    ###
    # 
    setBookmarkExclude(c("bookmark"))
    observeEvent(input$bookmark, {
        session$doBookmark()
    })
    
    observeEvent(input$reset, {
        
        # update menus
        updateSelectInput(session, 
                          inputId  = "river",
                          label    = "Fluss:",
                          choices  = rivers,
                          selected = "Bitte wählen Sie!"
        )
        
        output$area <- renderUI({})
        output$seq <- renderUI({})
        output$seq_gs <- renderPlot({})
        output$email <- renderUI({})
        output$submit <- renderUI({})
        output$reset <- renderUI({})
        
        # reset menu_elements
        menu_elements$river <- 0
        menu_elements$from_to <- 0
        menu_elements$area <- 0
        menu_elements$seq <- 0 
        menu_elements$email <- 0
        menu_elements$submit <- 0
        
        # reactivate js menu items
        enable("river")
        enable("from_to")
        enable("area")
        enable("seq")
        #enable("email")
        #enable("submit")
        
        # remove area polygons from map
        l <- leafletProxy("map")
        l %>% removeShape(layerId = c("area_tmp", "area"))
        
    })
    
    # onBookmark
    onBookmark(function(state) {
        state$menu_elements <- menu_elements
        state$res_elements <- res_elements
    })
    
    # onRestore
    onRestore(function(state) {
        menu_elements <- state$menu_elements
        res_elements <- state$res_elements
    })
    
    # onRestored
    # onRestored(
    #     
    # )
}
