library(shiny)

function(input, output, session) {
    
    # responsive menu
    output$menu_date <- renderUI({
        
        req(input$daterange)
        
        alignRight(sliderInput("date", "Zeit:",
                    min = as.Date(input$daterange[1]),
                    max = as.Date(input$daterange[2]),
                    value = input$daterange[1], step = 1, 
                    timeFormat = "%d.%m.%Y", ticks = FALSE, animate = TRUE, 
                    animationOptions(interval = 1000, loop = TRUE)
        ))
        
    })
    
    # title
    output$title <- renderText({
        req(input$date)
        strftime(input$date, "%d.%m.%Y")
    })
    
    # reactive df.gauging_data
    df.gdr <- reactive({
        
        req(input$gauging_station)
        req(input$daterange)
        
        df.gd[which(df.gd$gauging_station == input$gauging_station &
                        (df.gd$date >= as.Date(input$daterange[1]) &
                         df.gd$date <= as.Date(input$daterange[2]))), ]
    })
    
    # responsive plot
    output$plot <- renderPlot({
        
        req(input$gauging_station)
        req(input$daterange)
        req(input$date)
        
        # row_id in df.gdr()
        id_gd <- which(df.gdr()$date == input$date)
        
        # row_id in df.gsd
        id_gsd <- which(df.gsd$gauging_station == input$gauging_station)
        
        # recompute W to be relative to NHN
        W <- df.gdr()$w/100 + df.gsd$pnp[id_gsd]
        
        # plot
        par(oma = c(0, 0, 0, 0), mar = c(0, 6.5, 0, 0), xaxt = "n", xaxs = "i")
        plot(W ~ df.gdr()$date, type = "l", col = "darkblue",
             xlab = "Zeit", ylab = "Wasserstand (m über NHN)",
             frame.plot = FALSE)
        abline(h = df.gsd$pnp[id_gsd] + df.gsd$mw[id_gsd],
               lty = 3)
        abline(v = df.gdr()$date[id_gd], lty = 3, lwd = 0.5, col = "darkblue")
        abline(h = W[id_gd], lty = 3, lwd = 0.5, col = "darkblue")
        points(df.gdr()$date[id_gd], W[id_gd], col = "darkblue",
               pch = 20, cex = 2)
        boxed.labels(min(df.gdr()$date) + (max(df.gdr()$date) -
                                              min(df.gdr()$date)) * 0.95,
                     df.gsd$pnp[id_gsd] + df.gsd$mw[id_gsd],
                     "MW", bg = "white", border = FALSE, xpad = 1.0)
    })
    
    # responsive image
    output$image <- renderImage({
        
        req(input$gauging_station)
        req(input$date)
        
        image_path <- paste0("www/images/", input$gauging_station, "/flood3_",
                             strftime(input$date, "%Y%m%d"), ".png")
        if (!file.exists(image_path)){
            return(list(
                src = paste0("www/images/", input$gauging_station, "/dem.png"),
                contentType = "image/png"
            ))
        } else {
            return(list(
                    src = image_path,
                    contentType = "image/png"
                )
            )
        }
    }, deleteFile = FALSE)
    
}
