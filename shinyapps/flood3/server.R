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
    
    # responsive menu
    observe({
        id <- which(df.from_to$river == input$river)
        
        updateSliderInput(session,
                          inputId = "from_to",
                          label   = "Kilometer (von - bis):",
                          min     = df.from_to$from[id],
                          max     = df.from_to$to[id],
                          value   = c(df.from_to$from[id],
                                      df.from_to$to[id]),
                          step    = 0.1
        )
    })
    
    output$map <- renderLeaflet({
        leaflet() %>% addTiles() %>% 
            fitBounds(min(c(coor.e$lon, coor.r$lon)), 
                      min(c(coor.e$lat, coor.r$lat)), 
                      max(c(coor.e$lon, coor.r$lon)), 
                      max(c(coor.e$lat, coor.r$lat)))
        # setView(lng = -93.85, lat = 37.45, zoom = 4)
    })
    
    observe({
         l <- leafletProxy("map")
         l %>% clearShapes()
         if (input$river == "Elbe") {
             l %>% addPolygons(lng = coor.e$lon, lat = coor.e$lat,
                               label = "Elbe", color = "black", weight = 2,
                               fill = TRUE, fillColor = "lightblue", fillOpacity = 0.6)
             l %>% addPolygons(lng = coor.r$lon, lat = coor.r$lat,
                               label = "Rhein", color = "blue", weight = 1,
                               fill = TRUE, fillColor = "lightblue", fillOpacity = 0.2)
         } else {
             l %>% addPolygons(lng = coor.e$lon, lat = coor.e$lat,
                               label = "Elbe", color = "blue", weight = 1,
                               fill = TRUE, fillColor = "lightblue", fillOpacity = 0.2)
             l %>% addPolygons(lng = coor.r$lon, lat = coor.r$lat,
                               label = "Rhein", color = "black", weight = 2,
                               fill = TRUE, fillColor = "lightblue", fillOpacity = 0.6)
         }
         
         l %>% addDrawToolbar(targetGroup = "test",
                             circleOptions = FALSE,
                             polygonOptions = FALSE,
                             polylineOptions = FALSE,
                             markerOptions = FALSE,
                             circleMarkerOptions = FALSE,
                             editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))
    })
    
    # A reactive expression that returns the set of station_int that are
    # in bounds right now
    # station_intInBounds <- reactive({
    #     if (is.null(input$map_bounds))
    #         return(station_int[FALSE,])
    #     bounds <- input$map_bounds
    #     latRng <- range(bounds$north, bounds$south)
    #     lngRng <- range(bounds$east, bounds$west)
    #     
    #     subset(station_int,
    #            latitude >= latRng[1] & latitude <= latRng[2] &
    #                longitude >= lngRng[1] & longitude <= lngRng[2])
    # })
    
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
