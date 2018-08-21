library(shiny, lib.loc = lib)
library(leaflet, lib.loc = lib)
library(leaflet.extras, lib.loc = lib)
library(sp, lib.loc = lib)
library(raster, lib.loc = lib)
library(rgdal, lib.loc = lib)
library(rgeos, lib.loc = lib)
library(hyd1d, lib.loc = lib)
library(hydflood3, lib.loc = lib)

function(input, output, session) {
    
    # responsive df.from_to_reactive
    df.from_to_reactive <- reactive({
        id <- which(df.from_to$river == input$river)
        df.from_to_sel <- df.from_to[id, ]
        return(df.from_to_sel)
    })
    
    # responsive menu
    observe({
        # based on input$river
        df.from_to_sel <- df.from_to_reactive()
        
        # modify df.from_to_sel based on map bounds
        if (nrow(stationInBounds()) > 0) {
            df.from_to_sel$from <- min(stationInBounds()$station)
            df.from_to_sel$to <-  max(stationInBounds()$station)
        }
        
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
    
    output$map <- renderLeaflet({
        leaflet() %>% addTiles()
    })
    
    # A reactive expression that returns the set of hectometers that are
    # in bounds or in selected river or river section right now
    stationInBounds <- reactive({
        if (is.null(input$map_bounds))
            return(spdf.h[FALSE,])
        bounds <- input$map_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        if (input$river != "Bitte wählen Sie!") {
            subset(spdf.h,
                   latitude >= latRng[1] & latitude <= latRng[2] &
                   longitude >= lngRng[1] & longitude <= lngRng[2] &
                   river == input$river & 
                   station >= input$from_to[1] & station <= input$from_to[2])
        } else {
            subset(spdf.h,
                   latitude >= latRng[1] & latitude <= latRng[2] &
                   longitude >= lngRng[1] & longitude <= lngRng[2])
        }
    })
    
    observe({
         l <- leafletProxy("map")
         if (input$river == "Elbe") {
             l %>% removeShape(layerId = c("afe", "afr"))
             l %>% addPolygons(lng = coor.e$lon, lat = coor.e$lat,
                               label = "Elbe", color = "blue", weight = 2,
                               fill = TRUE, fillColor = "lightblue", 
                               fillOpacity = 0.6, layerId = "afe")
             l %>% addPolygons(lng = coor.r$lon, lat = coor.r$lat,
                               label = "Rhein", color = "blue", weight = 0.5,
                               fill = TRUE, fillColor = "lightblue", 
                               fillOpacity = 0.2, layerId = "afr")
         } else if (input$river == "Rhein") {
             l %>% removeShape(layerId = c("afe", "afr"))
             l %>% addPolygons(lng = coor.e$lon, lat = coor.e$lat,
                               label = "Elbe", color = "blue", weight = 0.5,
                               fill = TRUE, fillColor = "lightblue", 
                               fillOpacity = 0.2, layerId = "afe")
             l %>% addPolygons(lng = coor.r$lon, lat = coor.r$lat,
                               label = "Rhein", color = "blue", weight = 2,
                               fill = TRUE, fillColor = "lightblue", 
                               fillOpacity = 0.6, layerId = "afr")
         } else {
             l %>% removeShape(layerId = c("afe", "afr"))
             l %>% addPolygons(lng = coor.e$lon, lat = coor.e$lat,
                               label = "Elbe", color = "blue", weight = 2,
                               fill = TRUE, fillColor = "lightblue", 
                               fillOpacity = 0.6, layerId = "afe")
             l %>% addPolygons(lng = coor.r$lon, lat = coor.r$lat,
                               label = "Rhein", color = "blue", weight = 2,
                               fill = TRUE, fillColor = "lightblue", 
                               fillOpacity = 0.6, layerId = "afr")
         }
         
         if (nrow(stationInBounds()) != 0) {
             l %>% removeMarkerCluster(layerId = "station")
             l %>% flyToBounds(extent(stationInBounds())@xmin, 
                               extent(stationInBounds())@ymin, 
                               extent(stationInBounds())@xmax, 
                               extent(stationInBounds())@ymax)
             l %>% addMarkers(lng = stationInBounds()$longitude[c(1, nrow(stationInBounds()))],
                              lat = stationInBounds()$latitude[c(1, nrow(stationInBounds()))],
                              label = as.character(stationInBounds()$station[c(1, nrow(stationInBounds()))]),
                              layerId = "station")
         } else {
             l %>% fitBounds(extent(spdf.h)@xmin, 
                             extent(spdf.h)@ymin, 
                             extent(spdf.h)@xmax, 
                             extent(spdf.h)@ymax)
         }
         
         l %>% addDrawToolbar(targetGroup = "test",
                             circleOptions = FALSE,
                             polygonOptions = FALSE,
                             polylineOptions = FALSE,
                             markerOptions = FALSE,
                             circleMarkerOptions = FALSE,
                             editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))
    })
    
    observe(length(input$map_draw_all_features) == 0, {
        output$area <- renderText(paste0("Grenzen Sie den Berechnungsabschnitt",
                                         " ein!"))
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
    
    output$slope <- renderPlot({
        plot(1, 1, xlab = "km", ylab = "m über NHN (DHHN 92)")
    })
    
    # observe({
    #     id <- which(df.from_to$river == input$river)
    #     
    #     l <- leafletProxy("map")
    #     if (! all(input$from_to == c(df.from_to$from[id], df.from_to$to[id]))) {
    #         l %>% addPolygons(lng, lat, )
    #     } else {
    #         l %>% removeShapes("map", layerId = "station")
    #     }
    # })
    
    
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
