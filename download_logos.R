source("utils_helpers.R")

dir.create("www/logos", showWarnings = FALSE)

Map(downloadLogo, teamIds())
