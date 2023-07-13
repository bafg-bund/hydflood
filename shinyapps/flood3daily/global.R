# load the necessary packages
library(shiny)
library(shinyjs)
library(shiny.i18n)
library(hyd1d)
library(plotrix)

gs <- c("Elsnig", "Bösewig", "DESSAU", "Löderitz", "Schönberg-Deich", "LENZEN", "Jasebeck")

gsi <- c("TORGAU" = "Elsnig", "PRETZSCH-MAUKEN" = "Bösewig",
         "DESSAU" = "DESSAU", "AKEN" = "Löderitz", "SANDAU" = "Schönberg-Deich",
         "LENZEN" = "LENZEN", "DAMNATZ" = "Jasebeck")

gss <- c("eb1_Elsnig" = "Elsnig", "eb2_Boesewig" = "Bösewig",
         "DESSAU" = "DESSAU", "LOEDERITZ" = "Löderitz",
         "eb3_Schoenberg_Deich" = "Schönberg-Deich",
         "LENZEN" = "LENZEN", "eb4_Jasebeck" = "Jasebeck")

# access and query df.gauging_data
df.gd <- hyd1d:::.pkgenv$.df.gauging_data
id <- which(df.gd$gauging_station %in% names(gsi) &
                df.gd$date >= as.Date("2015-01-01"))
df.gd <- df.gd[id, ]

# date_max
date_max <- Sys.Date() - 2

# access and query df.gauging_station_data
id <- which(df.gauging_station_data$gauging_station %in% names(gsi))
df.gsd <- df.gauging_station_data[id, ]

# https://groups.google.com/forum/#!topic/shiny-discuss/gj-PDGH0KuM
alignRight <- function(el) {
    htmltools::tagAppendAttributes(el,
                                   style="margin-left:auto;margin-right:none;"
    )
}

# translation
translator <- Translator$new(translation_json_path = "translation.json")

# JavaScript to determine browser language
jscode <- paste0("var language =  window.navigator.userLanguage || window.navi",
                 "gator.language;Shiny.onInputChange('lang', language);console",
                 ".log(language);")
de <- function(x) {
    if (is.null(x)) {return(FALSE)}
    if (startsWith(x, "de")) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
