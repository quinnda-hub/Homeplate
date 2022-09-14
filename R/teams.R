# this function will return the integer team id for all teams if given no
# arguments, or any number of teams if they are passed in abbreviated form.
# Example: team_ids("TOR") returns a list of length 1, value 141
teamIds <- function(...) {
  ids <- list("ARI" = 109, "ATL" = 144, "BAL" = 110, "BOS" = 111, "CHC" = 112,
              "CHW" = 145, "CIN" = 113, "CLE" = 114, "COL" = 115, "DET" = 116,
              "HOU" = 117, "KCR" = 118, "LAA" = 108, "LAD" = 119, "MIA" = 146,
              "MIL" = 158, "MIN" = 142, "NYM" = 121, "NYY" = 147, "OAK" = 133,
              "PHI" = 143, "PIT" = 134, "SDP" = 135, "SFG" = 137, "SEA" = 136,
              "STL" = 138, "TBR" = 139, "TEX" = 140, "TOR" = 141, "WSN" = 120)

  if (missing(...)) {
    ids
  } else {
    ids[...]
  }
}

# this function will gather stats per team
.teamStats <- function(season, group = "hitting") {
   stat <- c("season", "seasonAdvanced")
   base <- "https://statsapi.mlb.com/api/"
   end <- paste0("v1/teams/stats?season=", season, "&stats=", stat, "&group=",
                group, "&sportIds=1")

   calls <- paste0(base, end)

   stats <- Map(\(x) GET(x) |> content(), calls)

   aux <- function(team_stats) {
    sapply(team_stats[["stats"]][[1]][["splits"]],
           \(i) {c(list(team = i[["team"]][["name"]]),
                   list(team_id = i[["team"]][["id"]]),
                   i[["stat"]])}, simplify = FALSE) |> rbindlist()
  }

   stats <- Map(aux, stats)
   Reduce(\(...) merge(..., by = "team_id", all = TRUE), stats)
}

teamStats <- function(season, group = "hitting") {
  stats <- .teamStats(season = season, group = group)
  stats <- stats[, which(!duplicated(t(stats))), with = FALSE] |>
    janitor::clean_names()
  setnames(stats, names(stats), gsub("_x", "", names(stats)))
  stats
}
