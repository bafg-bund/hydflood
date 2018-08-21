library(shiny, lib.loc = lib)

bootstrapPage(
    
    tags$head(
        # Include our custom CSS
        includeCSS("styles.css")
    ),
    
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    
    leafletOutput("map", width = "100%", height = "100%"),
    
    # Shiny versions prior to 0.11 should use class = "modal" instead.
    # Modified after https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/ui.R
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                  width = 330, height = "auto",
        
        h2("hydflood3::flood3()"),
        
        #####
        # menu item 1: river
        selectInput(
            inputId  = "river",
            label    = "Fluss:",
            choices  = rivers,
            selected = "Bitte wählen Sie!"
        ),
        
        #####
        # menu item 2: from_to
        conditionalPanel(
            "input.river != 'Bitte wählen Sie!'",
            sliderInput(
                inputId = "from_to", 
                label   = "Abschnitt (von km - bis km):",
                min     = df.from_to$from_val[1],
                max     = df.from_to$to_val[1],
                value   = c(df.from_to$from[1], df.from_to$to[1]),
                step    = 0.1
            )
        )
        
        #####
        # menu item 3: area
        # conditionalPanel(
        #     "(input.from_to[1] != 0 | input.from_to[1] != 336.2) & (input.from_to[2] != 585.7 | input.from_to[2] != 865.7)",
        #     
        #     p("Fläche (max. 2500 ha):"),
        #     
        #     textOutput("area"),
        #     
        #     # Draw sf.area
        #     x
        # ),
        
        #####
        # menu item 4: seq
        # conditionalPanel(
        #     condition = "!is.null(spdf.area_verified)",
        #     dateRangeInput(
        #         inputId = "seq",
        #         label = "Zeitraum:",
        #         start = "2017-01-01",
        #         end = "2017-12-31",
        #         min = "1990-01-01",
        #         max = as.character(Sys.Date() - 1),
        #         format = "dd.mm.yyyy",
        #         startview = "year",
        #         weekstart = 1,
        #         language = "de",
        #         separator = " bis "
        #     )
        # ),
        
        #####
        # menu item 5: plot W time series for uppermost spdf.gs_reactive()
        # menu item 6: query email adress
        # conditionalPanel(
        #     condition = "!is.null(seq_verified)",
        #     
        #     plotOutput("gauging_data", height = 200),
        #     
        #     textInput(
        #         inputId = "email",
        #         label = "Email:"),
        #     
        #     textOutput("email_validated")
        # ),
        
        #####
        # menu item 7: submit button
        # conditionalPanel(
        #     "input.email_validate == TRUE & input.submit == FALSE",
        #     actionButton(
        #         inputId = "submit",
        #         text = "Start der Berechung")
        # ),
        
        #####
        # menu item 8: confirm the processing | return smtp error message
        # conditionalPanel(
        #     "input.submit == TRUE",
        #     textOutput("submitted")
        # )
    )
)
