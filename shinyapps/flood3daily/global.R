# load the necessary packages
library(shiny)
library(hyd1d)
library(plotrix)

gs <- c("Elsnig", "Bösewig", "DESSAU", "Schönberg-Deich", "LENZEN", "Jasebeck")

gsi <- c("TORGAU" = "Elsnig", "PRETZSCH-MAUKEN" = "Bösewig",
         "DESSAU" = "DESSAU", "SANDAU" = "Schönberg-Deich",
         "LENZEN" = "LENZEN", "DAMNATZ" = "Jasebeck")

gss <- c("eb1_Elsnig" = "Elsnig", "eb2_Boesewig" = "Bösewig",
         "DESSAU" = "DESSAU", "eb3_Schoenberg_Deich" = "Schönberg-Deich",
         "LENZEN" = "LENZEN", "eb4_Jasebeck" = "Jasebeck")

# access and query df.gauging_data
id <- which(.df.gauging_data$gauging_station %in% names(gsi) &
                .df.gauging_data$date >= as.Date("2015-01-01"))
df.gd <- .df.gauging_data[id, ]

# date_max
date_max <- max(.df.gauging_data$date)

# access and query df.gauging_station_data
id <- which(df.gauging_station_data$gauging_station %in% names(gsi))
df.gsd <- df.gauging_station_data[id, ]

# https://groups.google.com/forum/#!topic/shiny-discuss/gj-PDGH0KuM
alignRight <- function(el) {
    htmltools::tagAppendAttributes(el,
                                   style="margin-left:auto;margin-right:none;"
    )
}
