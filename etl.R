library(data.table)
library(httr)
library(futile.logger)

source("utils_helpers.R")
source("teams.R")
source("players.R")
source("stats.R")

# Initialize the folder for data feeds.
dir.create("www/data", FALSE)

active_players <- teamIds() |> getRoster()

saveRDS(active_players, "www/data/active_players.rds")

saveRDS(active_players$player_id |> playerInfo(), "www/data/player_info.rds")

saveRDS(active_players$player_id |> gameLogs(format(Sys.Date(), "%Y")),
        "www/data/batting_logs.rds")

saveRDS(active_players$player_id |> gameLogs(format(Sys.Date(), "%Y"), "pitching"),
        "www/data/pitching_logs.rds")

saveRDS(active_players$player_id |> yearStats(), "www/data/batting_stats.rds")

saveRDS(active_players$player_id |> yearStats("pitching"), "www/data/pitching_stats.rds")
