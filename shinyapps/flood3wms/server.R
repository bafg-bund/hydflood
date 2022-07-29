################################################################################
# server.R
################################################################################

function(input, output, session) {
    
    # determine browser language
    runjs(jscode)
    german <- reactive({de(input$lang)})
    i18n <- reactive({
        if (length(input$lang) > 0 && german()) {
            translator$set_translation_language("de")
        } else {
            translator$set_translation_language("en")
        }
        translator
    })
    
    # reactive values to control the UI and finally the computation
    res <- reactiveValues(
        river = NA_character_, 
        from_to = c(NA_real_, NA_real_), 
        map_from_to = c(NA_real_, NA_real_), 
        river_from_to = c(NA_real_, NA_real_), 
        extent = c(NA_real_, NA_real_, NA_real_, NA_real_), 
        crs = NA_character_
    )
    
    # basemap
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
    
    ###
    # respond to the map inputs
    map_extent_reactive <- reactive({
        if (is.null(input$map_bounds)) {
            return(NULL)
        } else {
            return(extent(input$map_bounds$west, input$map_bounds$east,
                          input$map_bounds$south, input$map_bounds$north))
        }
    })
    
    #####
    # create menu item 1
    observe({
        if (is.null(input$river)) {
            output$river <- renderUI(
                selectInput(
                    inputId  = "river",
                    label    = i18n()$t("River:"),
                    choices  = rivers,
                    selected = "Please select!"
                )
            )
        } else {
            if (input$river != "Please select!") {
                res$river <- input$river
                res$from_to <- c(NA_real_, NA_real_)
                res$map_from_to <- c(NA_real_, NA_real_)
                res$river_from_to <- c(NA_real_, NA_real_)
                res$crs <- crs_reactive()
            } else {
                res$river <- NA_character_
                res$from_to <- c(NA_real_, NA_real_)
                res$map_from_to <- c(NA_real_, NA_real_)
                res$river_from_to <- c(NA_real_, NA_real_)
                res$crs <- NA_character_
            }
            
            output$river <- renderUI(
                selectInput(
                    inputId  = "river",
                    label    = i18n()$t("River:"),
                    choices  = rivers,
                    selected = res$river
                )
            )
        }
    }, priority = 10)
    
    #####
    # respond to menu item 1: input$river
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
            } else if (res$river == "Rhine") {
                return(CRS("+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs"))
            } else {
                return(NA_character_)
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
            } else if (res$river == "Rhine") {
                return(spdf.afr)
            } else {
                return(NULL)
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
            } else if (res$river == "Rhine") {
                return(spdf.active_floodplain_rhine)
            } else {
                return(NULL)
            }
        }
    })
    
    # responsive spdf.gsd
    spdf.gsd_reactive <- reactive({
        if (is.na(res$river)) {
            return(NULL)
        } else {
            return(spdf.gsd[which(spdf.gsd$river == toupper(res$river)), ])
        }
    })
    
    ###
    # observe changes of input$river and depending elements
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
                              label = i18n()$t("Rhine"), color = "blue", weight = 2,
                              fill = TRUE, fillColor = "lightblue", 
                              fillOpacity = 0.6, layerId = "afr")
        } else {
            if (res$river == "Elbe") {
                l %>% removeShape(layerId = c("afr"))
                l %>% addPolygons(lng = df.coor.afe$lon, lat = df.coor.afe$lat,
                                  label = "Elbe", color = "blue", weight = 2,
                                  fill = TRUE, fillColor = "lightblue", 
                                  fillOpacity = 0.6, layerId = "afe")
            } else if (res$river == "Rhine") {
                l %>% removeShape(layerId = c("afe"))
                l %>% addPolygons(lng = df.coor.afr$lon, lat = df.coor.afr$lat,
                                  label = i18n()$t("Rhine"), color = "blue", weight = 2,
                                  fill = TRUE, fillColor = "lightblue", 
                                  fillOpacity = 0.6, layerId = "afr")
            } else {
                l %>% addPolygons(lng = df.coor.afe$lon, lat = df.coor.afe$lat,
                                  label = "Elbe", color = "blue", weight = 2,
                                  fill = TRUE, fillColor = "lightblue", 
                                  fillOpacity = 0.6, layerId = "afe")
                l %>% addPolygons(lng = df.coor.afr$lon, lat = df.coor.afr$lat,
                                  label = i18n()$t("Rhine"), color = "blue", weight = 2,
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
        if (is.null(from_to_reactive())) {
            output$from_to <- renderUI("")
        } else {
            isolate(
                output$from_to <- renderUI({
                    disabled(
                        sliderInput(
                            inputId = "from_to",
                            label   = i18n()$t("Section (from km - to km)"),
                            min     = df.from_to_reactive()$from_val,
                            max     = df.from_to_reactive()$to_val,
                            value   = from_to_reactive(),
                            step    = 0.1
                        )
                    )
                })
            )
        }
    }, priority = 9)
    
    #####
    # respond to menu item 1 & 2: input$river & input$from_to
    ###
    # respond to input$from_to
    observeEvent(input$from_to, {
        river_from_to <- c(df.from_to_reactive()$from_val,
                           df.from_to_reactive()$to_val)
        if (any(input$from_to != river_from_to)) {
            res$from_to <- input$from_to
            res$map_from_to <- c(min(spdf.stationInBounds_reactive()$station),
                                 max(spdf.stationInBounds_reactive()$station))
            res$river_from_to <- river_from_to
        }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    ###
    # responsive datasets
    ##
    # responsive spdf.station
    spdf.stationFromTo_reactive <- reactive({
        if (is.na(res$river)) {
            return(spdf.station)
        } else {
            if (is.null(input$from_to) | all(is.na(res$from_to))) {
                return(spdf.station[
                    which(spdf.station$river == res$river), ])
            } else {
                return(spdf.station[
                    which(spdf.station$river == res$river &
                              spdf.station$station >= res$from_to[1] &
                              spdf.station$station <= res$from_to[2]), ])
            }
        }
    })
    
    # A reactive expression that returns the set of stations that are
    # in bounds right now
    spdf.stationInBounds_reactive <- reactive({
        if (is.null(input$map_bounds)) {
            return(spdf.station[FALSE,])
        }
        if (is.na(res$river)) {
            return(spdf.station[FALSE,])
        }
        bounds <- input$map_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(spdf.station,
               river == res$river &
                   latitude >= latRng[1] & latitude <= latRng[2] &
                   longitude >= lngRng[1] & longitude <= lngRng[2])
    })
    
    observeEvent(spdf.stationInBounds_reactive(), {
        if (nrow(spdf.stationInBounds_reactive()) > 0) {
            res$map_from_to <- c(min(spdf.stationInBounds_reactive()$station),
                                 max(spdf.stationInBounds_reactive()$station))
        }
    })
    
    # from_to
    from_to_reactive <- reactive({
        if (is.na(res$river)) {
            return(NULL)
        } else {
            if (all(is.na(res$from_to)) & all(is.na(res$map_from_to)) &
                all(is.na(res$river_from_to))) {
                from_to <- c(df.from_to_reactive()$from_val,
                             df.from_to_reactive()$to_val)
                res$from_to <- from_to
                res$map_from_to <- c(min(spdf.stationInBounds_reactive()$station),
                                     max(spdf.stationInBounds_reactive()$station))
                res$river_from_to <- from_to
                return(from_to)
            } else {
                if (all(res$from_to == res$river_from_to) &
                    all(res$from_to == res$map_from_to) & 
                    all(res$from_to == c(df.from_to_reactive()$from_val,
                                         df.from_to_reactive()$to_val))) {
                    return(res$from_to)
                } else {
                    
                    # permitted relative difference
                    dif <- (res$from_to[2] - res$from_to[1])/ 10
                    
                    # check differences
                    if (res$map_from_to[1] < res$from_to[1] - dif |
                        res$map_from_to[1] > res$from_to[1] + dif) {
                        from_to_lo <- res$map_from_to[1]
                    } else {
                        from_to_lo <- res$from_to[1]
                    }
                    if (res$map_from_to[2] < res$from_to[2] - dif |
                        res$map_from_to[2] > res$from_to[2] + dif) {
                        from_to_up <- res$map_from_to[2]
                    } else {
                        from_to_up <- res$from_to[2]
                    }
                    
                    res$from_to <- c(from_to_lo, from_to_up)
                    res$map_from_to <- c(from_to_lo, from_to_up)
                    return(c(from_to_lo, from_to_up))
                }
            }
        }
    })
    
    ##
    # modify background's extent
    # observe({
    #     if (nrow(spdf.stationFromTo_reactive()) != 0 &
    #         nrow(spdf.stationInBounds_reactive()) != 0) {
    #         station_a <- spdf.stationFromTo_reactive()$station
    #         station_b <- spdf.stationInBounds_reactive()$station
    #         
    #         l <- leafletProxy("map")
    #         
    #         if (length(station_a) == 1) {
    #             if (length(station_b) == 1) {
    #                 stations <- intersect(station_a, station_b)
    #                 if (length(stations) != 1) {
    #                     isolate(
    #                         l %>% setView(
    #                             lng = spdf.stationFromTo_reactive()$longitude,
    #                             lat = spdf.stationFromTo_reactive()$latitude,
    #                             zoom = 16)
    #                     )
    #                 }
    #             } else if (length(station_b) > 1) {
    #                 stations <- intersect(station_a, station_b)
    #                 if (length(stations) < 1) {
    #                     isolate(
    #                         l %>% setView(
    #                             lng = spdf.stationFromTo_reactive()$longitude,
    #                             lat = spdf.stationFromTo_reactive()$latitude,
    #                             zoom = 16)
    #                     )
    #                 } else if (length(stations) / length(station_b) < 1/10) {
    #                     spdf.station_sel <- spdf.stationInBounds_reactive()
    #                     id <- which(spdf.station_sel$station >= stations - 0.5 &
    #                                     spdf.station_sel$station <= stations + 0.5)
    #                     spdf.station_sel <- spdf.station_sel[id,]
    #                     isolate(
    #                         l %>% fitBounds(extent(spdf.station_sel)@xmin,
    #                                         extent(spdf.station_sel)@ymin,
    #                                         extent(spdf.station_sel)@xmax,
    #                                         extent(spdf.station_sel)@ymax)
    #                     )
    #                 }
    #             }
    #         } else if (length(station_a > 1)) {
    #             if (length(station_b) == 0) {
    #                 isolate(
    #                     l %>% fitBounds(
    #                         extent(spdf.stationFromTo_reactive())@xmin,
    #                         extent(spdf.stationFromTo_reactive())@ymin,
    #                         extent(spdf.stationFromTo_reactive())@xmax,
    #                         extent(spdf.stationFromTo_reactive())@ymax)
    #                 )
    #             } else if (length(station_b) == 1) {
    #                 stations <- intersect(station_a, station_b)
    #                 if (length(stations) != 1) {
    #                     isolate(
    #                         l %>% fitBounds(
    #                             extent(spdf.stationFromTo_reactive())@xmin,
    #                             extent(spdf.stationFromTo_reactive())@ymin,
    #                             extent(spdf.stationFromTo_reactive())@xmax,
    #                             extent(spdf.stationFromTo_reactive())@ymax)
    #                     )
    #                 }
    #             } else {
    #                 stations <- intersect(station_a, station_b)
    #                 
    #                 stations_a_in_map <- which(station_a %in% station_b)
    #                 stations_b_in_a <- which(station_b %in%station_a)
    #                 if (length(stations_a_in_map) / length(station_a) < 0.9 |
    #                     (length(stations_a_in_map) / length(station_a) == 1 &
    #                      (length(stations_b_in_a) / length(station_b) < 0.9 |
    #                       length(stations_b_in_a) / length(station_b) == 1))) {
    # 
    #                     station_center <- gCentroid(
    #                         spdf.stationFromTo_reactive())
    #                     map_center <- SpatialPoints(
    #                         as.data.frame(input$map_center), crs)
    #                     map_bounds <- input$map_bounds
    # 
    #                     if (!withinDist(x = station_center,
    #                                     y = map_center,
    #                                     bb = map_bounds)) {
    #                         isolate(
    #                             l %>% fitBounds(
    #                                 extent(spdf.stationFromTo_reactive())@xmin,
    #                                 extent(spdf.stationFromTo_reactive())@ymin,
    #                                 extent(spdf.stationFromTo_reactive())@xmax,
    #                                 extent(spdf.stationFromTo_reactive())@ymax)
    #                         )
    #                     }
    #                 } else {
    #                     
    #                     station_center <- gCentroid(
    #                         spdf.stationFromTo_reactive())
    #                     map_center <- SpatialPoints(
    #                         as.data.frame(input$map_center), crs)
    #                     map_bounds <- input$map_bounds
    #                     
    #                     if (!withinDist(x = station_center,
    #                                     y = map_center,
    #                                     bb = map_bounds)) {
    #                         isolate(
    #                             l %>% fitBounds(
    #                                 extent(spdf.stationFromTo_reactive())@xmin,
    #                                 extent(spdf.stationFromTo_reactive())@ymin,
    #                                 extent(spdf.stationFromTo_reactive())@xmax,
    #                                 extent(spdf.stationFromTo_reactive())@ymax)
    #                         )
    #                     }
    #                 }
    #             }
    #         }
    #     }
    # })
    
    #####
    # create or update the basic settings of menu item 3
    observe({
        if (is.na(res$river)) {
            output$year <- renderUI("")
        } else {
            if (all(is.na(res$from_to))) {
                output$year <- renderUI("")
            } else {
                if (is.null(input$year)) {
                    year <- as.numeric(strftime(Sys.Date(), "%Y")) - 1
                } else {
                    year <- input$year
                }
                output$year <- renderUI({
                    selectInput(
                        inputId  = "year",
                        label    = i18n()$t("Year:"),
                        choices  = years,
                        selected = year
                    )
                })
            }
        }
    }, priority = 8)
    
    #####
    # display the selected ufd map
    observe({
        if (!is.na(res$river) & 
            !is.null(input$year) & 
            !is.null(input$map_zoom)) {
            
            year <- as.numeric(input$year)
            
            l <- leafletProxy("map")
            # zoom level denpendent visualisation
            if (input$map_zoom >= 12) {
                url <- paste0(url_base, tolower(input$river), "_",
                              as.character(floor(year / 10) * 10),
                              "_",
                              as.character(floor(year / 10) * 10 + 9),
                              "/MapServer/WMSServer?")
                l %>% addWMSTiles(
                    baseUrl = url,
                    layers = paste0(input$river, "_flood3_",
                                    as.character(year)),
                    options = WMSTileOptions(format = "image/png",
                                             transparent = TRUE),
                    layerId = "fd")
                # url <- paste0(url_base, input$river, "_",
                #               as.character(floor(year / 10) * 10),
                #               "_",
                #               as.character(floor(year / 10) * 10 + 9),
                #               "/MapServer")
                # l %>% addEsriTiledMapLayer(url = url,
                #     options = tiledMapLayerOptions(
                #         tileOptions = WMSTileOptions(format = "image/png",
                #                                      transparent = TRUE)),
                #     layerId = as.character(year - (floor(year / 10) * 10)))
                output$legend <- renderText(
                    i18n()$t(
                        paste0("<p>Legend:</p><p><center>flood duration (d/y)<",
                               "/center></p><center><img width=\"40%\" style=",
                               "\"\" src=\"legend_en.png\"></center>")))
                
                if (res$river == "Elbe") {
                    l %>% removeShape(layerId = c("afr"))
                    l %>% addPolygons(lng = df.coor.afe$lon, 
                                      lat = df.coor.afe$lat,
                                      label = "Elbe", color = "blue", 
                                      weight = 2, fill = FALSE, layerId = "afe")
                } else if (res$river == "Rhine") {
                    l %>% removeShape(layerId = c("afe"))
                    l %>% addPolygons(lng = df.coor.afr$lon, 
                                      lat = df.coor.afr$lat,
                                      label = i18n()$t("Rhine"), color = "blue", 
                                      weight = 2, fill = FALSE, layerId = "afr")
                } else {
                    l %>% addPolygons(lng = df.coor.afe$lon, 
                                      lat = df.coor.afe$lat,
                                      label = "Elbe", color = "blue", 
                                      weight = 2, fill = FALSE, layerId = "afe")
                    l %>% addPolygons(lng = df.coor.afr$lon, 
                                      lat = df.coor.afr$lat,
                                      label = i18n()$t("Rhine"), color = "blue", 
                                      weight = 2, fill = FALSE, layerId = "afr")
                }
            } else {
                if (res$river == "Elbe") {
                    l %>% removeShape(layerId = c("afr"))
                    l %>% addPolygons(lng = df.coor.afe$lon, 
                                      lat = df.coor.afe$lat,
                                      label = "Elbe", color = "blue", 
                                      weight = 2, fill = TRUE, 
                                      fillColor = "lightblue", 
                                      fillOpacity = 0.6, layerId = "afe")
                } else if (res$river == "Rhine") {
                    l %>% removeShape(layerId = c("afe"))
                    l %>% addPolygons(lng = df.coor.afr$lon, 
                                      lat = df.coor.afr$lat,
                                      label = i18n()$t("Rhine"), color = "blue", 
                                      weight = 2, fill = TRUE, 
                                      fillColor = "lightblue", 
                                      fillOpacity = 0.6, layerId = "afr")
                } else {
                    l %>% addPolygons(lng = df.coor.afe$lon, 
                                      lat = df.coor.afe$lat,
                                      label = "Elbe", color = "blue", 
                                      weight = 2, fill = TRUE, 
                                      fillColor = "lightblue", 
                                      fillOpacity = 0.6, layerId = "afe")
                    l %>% addPolygons(lng = df.coor.afr$lon, 
                                      lat = df.coor.afr$lat,
                                      label = i18n()$t("Rhine"), color = "blue", 
                                      weight = 2, fill = TRUE, 
                                      fillColor = "lightblue", 
                                      fillOpacity = 0.6, layerId = "afr")
                }
                output$legend <- renderText("")
            }
        }
    })
    
    output$zoom <- renderText({
        if (!is.null(input$map_zoom) & !is.null(input$year)) {
            if (input$map_zoom < 12) {
                paste0(i18n()$t("<p>Zoomlevel:</p><p>Presently it is <strong>"),
                       input$map_zoom, 
                       i18n()$t(
                           paste0("</strong>, starting at <strong>12</strong>,",
                                  " the web map service of the flood duration ",
                                  "and the associated legend will be displayed",
                                  ".</p>")))
            } # else {
            #     paste0('<p>Zoomlevel:</p><p>',input$map_zoom, '</p>')
            # }
        }
    })
    
    
    output$imprint <- renderUI({
        tagList(a(i18n()$t("Imprint"), 
                  href = i18n()$t("https://www.bafg.de/EN/Service/Imprint/imprint_node.html")))
    })
    
}
