library(shiny, lib.loc = lib)
library(leaflet, lib.loc = lib)
library(leaflet.extras, lib.loc = lib)
library(sp, lib.loc = lib)
library(raster, lib.loc = lib)
library(rgdal, lib.loc = lib)
library(rgeos, lib.loc = lib)
library(hyd1d, lib.loc = lib)
library(hydflood3, lib.loc = lib)
library(mapedit, lib.loc = lib)
library(mapview, lib.loc = lib)

function(input, output, session) {
    
    # leaflet background map
    output$map <- renderLeaflet({
        leaflet() %>% addTiles()
    })
    
    # leafletProxy for responsive background of spdf.active_floodplain_* and
    # spdf.gauging_station
    observe({
        l <- leafletProxy("map")
        l %>% removeShape(layerId = c("afe", "afr")) #, "gs"))
        if (input$river == "Elbe") {
            l %>% addPolygons(lng = df.coor.afe$lon, lat = df.coor.afe$lat,
                              label = "Elbe", color = "blue", weight = 2,
                              fill = TRUE, fillColor = "lightblue", 
                              fillOpacity = 0.6, layerId = "afe")
            l %>% addPolygons(lng = df.coor.afr$lon, lat = df.coor.afr$lat,
                              label = "Rhein", color = "blue", weight = 0.5,
                              fill = TRUE, fillColor = "lightblue", 
                              fillOpacity = 0.2, layerId = "afr")
        } else if (input$river == "Rhein") {
            l %>% addPolygons(lng = df.coor.afe$lon, lat = df.coor.afe$lat,
                              label = "Elbe", color = "blue", weight = 0.5,
                              fill = TRUE, fillColor = "lightblue", 
                              fillOpacity = 0.2, layerId = "afe")
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
        
        # l %>% addMarkers(lng = spdf.gs_reactive()$longitude, lat = spdf.gs_reactive()$latitude, 
        #                  popup = htmlEscape(spdf.gs_reactive()$gauging_station), 
        #                  layerId = "gs")
        
    })
    
    #####
    # menu item 2
    # responsive df.from_to
    df.from_to_reactive <- reactive({
        df.from_to[which(df.from_to$river == input$river), ]
    })
    
    # a reactive expression that returns the set of stations that are
    # in bounds or in selected river or river section
    spdf.stationInBounds <- reactive({
        if (is.null(input$map_bounds))
            return(spdf.station[FALSE,])
        bounds <- input$map_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        if (input$river != "Bitte wählen Sie!") {
            subset(spdf.station,
                   latitude >= latRng[1] & latitude <= latRng[2] &
                       longitude >= lngRng[1] & longitude <= lngRng[2] &
                       river == input$river & 
                       station >= input$from_to[1] & station <= input$from_to[2])
        } else {
            subset(spdf.station,
                   latitude >= latRng[1] & latitude <= latRng[2] &
                       longitude >= lngRng[1] & longitude <= lngRng[2])
        }
    })
    
    # responsive menu items from_to
    observe({
        # based on input$river
        df.from_to_sel <- df.from_to_reactive()
        
        # modify df.from_to_sel based on map bounds
        # if (nrow(spdf.stationInBounds()) > 0) {
        #     df.from_to_sel$from <- min(spdf.stationInBounds()$station)
        #     df.from_to_sel$to <-  max(spdf.stationInBounds()$station)
        # }
        
        # update the slider
        updateSliderInput(session,
                          inputId = "from_to",
                          label   = "Kilometer (von - bis):",
                          min     = df.from_to_sel$from_val,
                          max     = df.from_to_sel$to_val,
                          value   = c(df.from_to_sel$from,
                                      df.from_to_sel$to),
                          step    = 0.1
        )
    })
    
    
    # leafletProxy for responsive background with spdf.gs_*
    spdf.gs_reactive <- reactive({
        spdf.gs_t <- spdf.gs[which(spdf.gs$river == input$river), ]
        id_up <- which(spdf.gs_t$km_qps < input$from_to[1])
        id_inarea <- which(spdf.gs_t$km_qps >= input$from_to[1] &
                           spdf.gs_t$km_qps <= input$from_to[2])
        id_do <- which(spdf.gs_t$km_qps > input$from_to[2])
        spdf.gs_t[unique(c(id_up, id_inarea,id_do)), ]
    })
    
    observe({
         l <- leafletProxy("map")
         l %>% removeShape(layerId = c("gs"))
         l %>% addMarkers(lng = spdf.gs_reactive()$longitude, 
                          lat = spdf.gs_reactive()$latitude, 
                          popup = htmlEscape(spdf.gs_reactive()$gauging_station), 
                          layerId = "gs")
    })
    
    # leafletProxy for responsive resizing of the background of spdf.active_floodplain_*
    observe({
        
        l <- leafletProxy("map")
        
        if (nrow(spdf.stationInBounds()) != 0) {
            
            l %>% removeMarkerCluster(layerId = "station")
            
            map.e <- extent(input$map_bounds$west, input$map_bounds$east,
                            input$map_bounds$south, input$map_bounds$north)
            if (map.e < extent(spdf.stationInBounds())) {
                l %>% flyToBounds(extent(spdf.stationInBounds())@xmin, 
                                  extent(spdf.stationInBounds())@ymin, 
                                  extent(spdf.stationInBounds())@xmax, 
                                  extent(spdf.stationInBounds())@ymax)
            }
            l %>% addMarkers(lng = spdf.stationInBounds()$longitude[c(1, nrow(spdf.stationInBounds()))],
                             lat = spdf.stationInBounds()$latitude[c(1, nrow(spdf.stationInBounds()))],
                             label = as.character(spdf.stationInBounds()$station[c(1, nrow(spdf.stationInBounds()))]),
                             layerId = "station")
        } else {
            l %>% fitBounds(extent(spdf.station)@xmin, 
                            extent(spdf.station)@ymin, 
                            extent(spdf.station)@xmax, 
                            extent(spdf.station)@ymax)
        }
        
        l %>% drawFeatures(sf = TRUE, record = FALSE,
                           viewer = shiny::paneViewer(), title = "Draw Features", ...)
         # l %>% addDrawToolbar(targetGroup = "test",
         #                     circleOptions = FALSE,
         #                     polygonOptions = FALSE,
         #                     polylineOptions = FALSE,
         #                     markerOptions = FALSE,
         #                     circleMarkerOptions = FALSE,
         #                     editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))
    })
    
    observe({
        is.null(sf.area){
            output$area <- renderText(paste0("Grenzen Sie den Berechnungsabschnitt",
                                             "mit der Rechteckmaske ein!"))
        } else {
            # check area size
            spdf.area <- as(sf.area, 'Spatial')
            area <- area(spdf.area)
            if (area > 5000000) {
                output$area <- renderText(paste0("Die gewählte Fläche beträgt ", 
                                                 area, " m² (", round(area / 10000,
                                                                      2), " ha)\n",
                                                 "und ist größer als die maximal ", 
                                                 "erlaubten 25000000 m² (2500 ha)",
                                                 ".\n Bitte reduzieren Sie die Fl",
                                                 "ächengröße."))
                spdf.area_verified <- NULL
            } else {
                output$area <- renderText(paste0("Die gewählte Fläche beträgt ", 
                                                 area, " m² (", round(area / 10000,
                                                                      2), " ha)."))
                spdf.area_verified <- spdf.area
            }
        }
    })
    
    #https://github.com/bhaskarvk/leaflet.extras/blob/master/inst/examples/shiny/draw-events/app.R
    # Start of Drawing
    observeEvent(input$map_draw_start, {
        print("Start of drawing")
        print(input$map_draw_start)
    })
    
    # Stop of Drawing
    observeEvent(input$map_draw_stop, {
        print("Stopped drawing")
        print(input$map_draw_stop)
    })
    
    # New Feature
    observeEvent(input$map_draw_new_feature, {
        print("New Feature")
        print(input$map_draw_new_feature)
    })
    
    # Edited Features
    observeEvent(input$map_draw_edited_features, {
        print("Edited Features")
        print(input$map_draw_edited_features)
    })
    
    # Deleted features
    observeEvent(input$map_draw_deleted_features, {
        print("Deleted Features")
        print(input$map_draw_deleted_features)
    })
    
    # We also listen for draw_all_features which is called anytime
    # features are created/edited/deleted from the map
    observeEvent(input$map_draw_all_features, {
        print("All Features")
        print(input$map_draw_all_features)
    })
    
    # observeEvent(input$map_draw_stop, {
    #     sp.area <- reactiveValues(input$map_draw_new_feature)
    #     output$area <- renderText(str(sp.area))
    #     print(input$map_draw_new_feature)
    # })
    
    ############################################################################
    #observe({
        #output$area <- renderText(as.character(input$map_drawnItems_created$geometry$coordinates))
        # if () {
        #     output$area <- renderText(paste0("Wählen Sie die gewünschte Berech",
        #                                      "nungs-fläche mit der Zeichenfunk",
        #                                      "tion ([]) aus!"))
        # } else if () {
        #     output$area <- renderText(paste0("Wählen Sie die gewünschte Berech",
        #                                      "nungs-fläche mit der Zeichenfunk",
        #                                      "tion ([]) aus!"))
        # } else {
        #     output$area <- renderText(paste0("Wählen Sie die gewünschte Berech",
        #                                      "nungs-fläche mit der Zeichenfunk",
        #                                      "tion ([]) aus!"))
        #}
    #})
    
    ############################################################################
    # data("df.gauging_station_data", package = "hyd1d", lib.loc = lib)
    # df.gsd <- df.gauging_station_data[df.gauging_station_data$data_present,]
    # 
    # gs_reactive <- reactive({
    #     # upper most computation-relevant gauging_station
    #     
    # })
    # 
    # pnp_reactive <- reactive({
    #     df.gsd[which(df.gsd$gauging_station == gs_reactive()), "pnp"]
    # })
    # 
    # df.gd_reactive <- reactive({
    #     df.gd[which(df.gd$gauging_station == gs_reactive &
    #                 df.gd$date >= input$seq[1] &
    #                 df.gd$date <= input$seq[2]), ]
    # })
    # 
    # output$gauging_data <- renderPlot({
    #     # par(oma = c(2, 2, 0.2, 0.2))
    #     plot(x = df.gd_reactive()$station, 
    #          y = (df.gd_reactive()$w / 100) + pnp_reactive(), 
    #          xlab = "km", ylab = "m über NHN (DHHN 92)")
    # })
    
    ############################################################################
    # observe input$email
    observe({
        if (is.null(email_valid)) {
            output$email_validated <- renderText("")
        } else {
            
        }
    }) 
    
    ############################################################################
    # observe submit
    observeEvent(input$submit, {
        # update
        updateActionButton(inputId = "submit", label = NULL, icon = NULL)
        
        output$submitted <- renderText("Hallo")
    })
    
    # # responsive downloadData
    # output$downloadData <- downloadHandler(filename = function(){
    #     paste0('waterLevel_', input$river, "_",
    #            strftime(input$time, format = "%Y%m%d"),
    #            '.csv')},
    #     content = function(file){
    #         
    #         data <- wldf()
    #         data <- as.data.frame(data)[, 1:3]
    #         
    #         write.table(data,
    #                     file,
    #                     quote = FALSE,
    #                     sep = ";",
    #                     dec = ",",
    #                     row.names = FALSE,
    #                     fileEncoding = "UTF-8")},
    #     contentType = "text/csv"
    # )
    
}
