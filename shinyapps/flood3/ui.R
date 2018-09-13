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
            
            h2("hydflood3::flood3()"),
            
            #####
            # menu item 1: river
            uiOutput("river"),
            
            #####
            # menu item 2: from_to
            uiOutput("from_to"),
            
            #####
            # menu item 3: area
            uiOutput("area"),
            
            #####
            # menu item 4: seq
            uiOutput("seq"),
            
            #####
            # menu item 5: plot W time series for uppermost spdf.gs_reactive()
            # menu item 6: query email adress
            plotOutput("seq_gs", inline = TRUE),
            uiOutput("email"),
           
            #####
            # menu item 7: submit or bookmark or restore or reset buttons
            uiOutput("submit")
            
        )
    )
}

