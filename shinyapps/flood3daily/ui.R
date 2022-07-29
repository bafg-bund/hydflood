fluidPage(
    
    tags$head(
        tags$style(HTML("#footer {
            font-weight: bold;
            margin-bottom: 10px;
        }"))
    ),
    
    column(width = 3,
        
        HTML("<BR>"),
        HTML("<BR>"),
        HTML("<BR>"),
        
        # menu items
        uiOutput("menu_area"),
        uiOutput("menu_daterange"),
        uiOutput("menu_date"),
        
        # plot 
        plotOutput("plot"),
        
        # imprint
        uiOutput("footer")
        
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
        
        htmlOutput("image")
    )
)
