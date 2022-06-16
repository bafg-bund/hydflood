library(shiny)

fluidPage(
    
    column(width = 3,
        
        HTML("<BR>"),
        HTML("<BR>"),
        HTML("<BR>"),
        
        # menu item gauging_station
        alignRight(selectInput(
            inputId  = "area",
            label    = "Bereich:",
            choices  = gs,
            selected = "DESSAU")),
        
        # menu item DATERANGE
        alignRight(dateRangeInput("daterange", "Zeitraum:", 
                       start = as.Date("2015-01-01"), end = date_max, 
                       min = as.Date("2015-01-01"), max = date_max, 
                       format = "dd-mm-yyyy",
                       language = "de", separator = " bis ")),
        
        # menu item DATE
        uiOutput("menu_date"),
        
        # plot 
        plotOutput("plot")
        
    ),
    
    column(width = 9,
           align = "center",
           h2(textOutput("title")),
        
        tags$style(
            type="text/css",
            "#image img {
                max-width: 90%;
                width: 90%; 
                height: auto;
            }"
        ),
        
        imageOutput("image")
    )
    
)
