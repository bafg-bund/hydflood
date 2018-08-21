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
                  
                  selectInput(
                      inputId  = "river",
                      label    = "Fluss:",
                      choices  = rivers,
                      selected = "Bitte wählen Sie!"
                  ),
                  
                  conditionalPanel("input.river != 'Bitte wählen Sie!'",
                      sliderInput(
                          inputId = "from_to", 
                          label   = "Abschnitt (von km - bis km):",
                          min     = df.from_to$from_val[1],
                          max     = df.from_to$to_val[1],
                          value   = c(df.from_to$from[1], df.from_to$to[1]),
                          step    = 0.1
                      ),
                      
                      p("Fläche:"),
                      
                      textOutput("area")
                  )
                  
                  
                  # conditional ...
                  # conditionalPanel(
                  #     condition = "1 == 1",
                  #   dateRangeInput(
                  #     inputId = "seq", 
                  #     label = "Zeitraum:",
                  #     start = "2017-01-01", 
                  #     end = "2017-12-31", 
                  #     min = "1990-01-01",
                  #     max = as.character(Sys.Date() - 1), 
                  #     format = "dd.mm.yyyy", 
                  #     startview = "year", 
                  #     weekstart = 1,
                  #     language = "de", 
                  #     separator = " bis "),
                  #   
                  #   plotOutput("slope", height = 200)
                  #   
                  # ),
                  # 
                  # textInput(
                  #     inputId = "email", 
                  #     label = "Email:"),
                  # 
                  # # textOutput("email_validate"),
                  # 
                  # p(paste0("Nach Abschluss der Berechnungen erhalten Sie eine ",
                  #          "Email mit einem Link für den Download der Ergebnis",
                  #          "se.")),
                  # 
                  # conditionalPanel(
                  #     "input.email_validate == TRUE", 
                  #     submitButton(text = "Start der Berechung")
                  # )
    )
)
