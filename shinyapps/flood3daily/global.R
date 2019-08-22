# load the necessary packages
library(shiny)
library(hyd1d)
library(plotrix)

gs <- c("DESSAU", "LENZEN")

# access and query df.gauging_data
df.gauging_data <- readRDS("~/.hyd1d/df.gauging_data_latest.RDS")
id <- which(df.gauging_data$gauging_station %in% gs &
                df.gauging_data$date >= as.Date("2015-01-01"))
df.gd <- df.gauging_data[id, ]

# date_max
date_max <- max(df.gauging_data$date)

# access and query df.gauging_station_data
id <- which(df.gauging_station_data$gauging_station %in% gs)
df.gsd <- df.gauging_station_data[id, ]

# https://groups.google.com/forum/#!topic/shiny-discuss/gj-PDGH0KuM
alignRight <- function(el) {
    htmltools::tagAppendAttributes(el,
                                   style="margin-left:auto;margin-right:none;"
    )
}
