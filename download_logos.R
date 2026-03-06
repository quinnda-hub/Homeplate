source("utils_helpers.R")
source("teams.R")

dir.create("www/logos", showWarnings = FALSE, recursive = TRUE)

Map(downloadLogo, teamIds())
