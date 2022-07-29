################################################################################
# ui.R
################################################################################

function(request) {
    bootstrapPage(
        
        tags$head(
            # Include our custom CSS
            includeCSS("styles.css")
        ),
        
        tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        
        useShinyjs(),
        
        leafletOutput("map", width = "100%", height = "100%"),
        
        # Modified after https://github.com/rstudio/shiny-examples/blob/master/063-
        # superzip-example/ui.R
        absolutePanel(
            id = "controls", class = "panel panel-default", fixed = TRUE,
            draggable = TRUE, left = "auto", right = 20, width = 330, 
            top = 20, bottom = "auto", height = "auto", 
            
            h2("flood3()-WMS"),
            
            #####
            # menu item 1: river
            uiOutput("river"),
            
            #####
            # menu item 2: from_to
            uiOutput("from_to"),
            
            #####
            # menu item 3: year
            uiOutput("year"),
            
            # zoom info
            htmlOutput("zoom"),
            
            uiOutput("extent"),
            
            # legend
            htmlOutput("legend"),
            
            # imprint
            uiOutput("imprint")
        )
    )
}

