# obtain meaningful error messages
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")
Sys.setlocale(category = "LC_PAPER", locale = "en_US.UTF-8")
Sys.setlocale(category = "LC_MEASUREMENT", locale = "en_US.UTF-8")
Sys.setlocale(category = "LC_MESSAGES", locale = "en_US.UTF-8")

# load rsconnect-package
library(rsconnect)

# deploy
# flood3daily
appDir <- "shinyapps/flood3daily"

files <- listDeploymentFiles(
    appDir = appDir,
    appFiles = NULL,
    appFileManifest = NULL,
    error_call = caller_env()
)

rsconnect::deployApp(
    appDir = appDir,
    appFiles = files,
    appName = "flood3daily",
    appTitle = "flood3daily",
    launch.browser = FALSE,
    logLevel = "verbose",
    forceUpdate = TRUE,
    appId = 6,
    appVisibility = "public",
    account = "arnd.weber@bafg.de",
    server = "shiny.bafg.de")

# flood3wms
appDir <- "shinyapps/flood3wms"

files <- listDeploymentFiles(
    appDir = appDir,
    appFiles = NULL,
    appFileManifest = NULL,
    error_call = caller_env()
)

rsconnect::deployApp(
    appDir = appDir,
    appFiles = files,
    appName = "flood3wms",
    appTitle = "flood3wms",
    launch.browser = FALSE,
    logLevel = "verbose",
    forceUpdate = TRUE,
    appId = 7,
    appVisibility = "public",
    account = "arnd.weber@bafg.de",
    server = "shiny.bafg.de")

