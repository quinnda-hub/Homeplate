source("utils_helpers.R")
source("teams.R")

dir.create("www/logos", showWarnings = FALSE)

Map(downloadLogo, teamIds())
