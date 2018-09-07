library(shiny, lib.loc = lib)
library(shinyjs, lib.loc = lib)

bootstrapPage(
    
    tags$head(
        # Include our custom CSS
        includeCSS("styles.css")
    ),
    
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    
    useShinyjs(),
    
    leafletOutput("map", width = "100%", height = "100%"),
    
    # Shiny versions prior to 0.11 should use class = "modal" instead.
    # Modified after https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/ui.R
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 20, left = "auto", right = 20, bottom = "auto",
                  width = 330, height = "auto",
        
        h2("hydflood3::flood3()"),
        
        #####
        # menu item 1: river
        uiOutput("river"),
        
        # selectInput(
        #     inputId  = "river",
        #     label    = "Fluss:",
        #     choices  = rivers,
        #     selected = "Bitte wählen Sie!"
        # ),
        
        #####
        # menu item 2: from_to
        uiOutput("from_to"),
        
        # conditionalPanel(
        #     "input.river != 'Bitte wählen Sie!'",
        #     sliderInput(
        #         inputId = "from_to",
        #         label   = "Abschnitt (von km - bis km):",
        #         min     = df.from_to$from_val[1],
        #         max     = df.from_to$to_val[1],
        #         value   = c(df.from_to$from[1], df.from_to$to[1]),
        #         step    = 0.1
        #     )
        # ),
        
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
        # menu item 7: submit button
        uiOutput("submit"),
        
        #####
        # menu item 8: bookmark or reset
        uiOutput("reset")
        
    )
)
