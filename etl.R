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

saveRDS(
  active_players$player_id |> gameLogs(getCurrentSeason()),
  "www/data/batting_logs.rds"
)

saveRDS(
  active_players$player_id |> gameLogs(getCurrentSeason(), "pitching"),
  "www/data/pitching_logs.rds"
)

batting_year_stats <- active_players$player_id |> yearStats()

pitching_year_stats <- active_players$player_id |> yearStats("pitching")

batting_sabermetrics <- playerSabermetricsMany(
  batting_year_stats,
  groups = "hitting"
)

pitching_sabermetrics <- playerSabermetricsMany(
  pitching_year_stats,
  groups = "pitching"
)

batting_year_stats <- batting_year_stats[
  batting_sabermetrics[, .(
    player_id,
    season,
    team_id,
    woba,
    w_rc_plus,
    base_running,
    fielding,
    positional,
    war
  )],
  on = .(id == player_id, season, team_id)
]

pitching_year_stats <- pitching_year_stats[
  pitching_sabermetrics[, .(
    player_id,
    season,
    team_id,
    fip,
    xfip,
    era_minus,
    war
  )],
  on = .(id == player_id, season, team_id)
]

saveRDS(batting_year_stats, "www/data/batting_stats.rds")
saveRDS(pitching_year_stats, "www/data/pitching_stats.rds")
