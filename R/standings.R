# this function will get the standings for the given season
.getStandings <- function(season) {
  base <- "https://statsapi.mlb.com/api/"
  end <- paste0("v1/standings", "?leagueId=", c(103, 104), "&season=", season)

  calls <- paste0(base, end) |> lapply(\(x) GET(x) |> content())

  lapply(calls, \(i) {
    sapply(i[["records"]], \(j) {
      sapply(j[["teamRecords"]], \(k) {
        team <- k[["team"]][["name"]];
        id <- k[["team"]][["id"]];
        wins <- k[["wins"]];
        losses <- k[["losses"]];
        w10 <- k[["records"]][["splitRecords"]][[9]][["wins"]];
        l10 <- k[["records"]][["splitRecords"]][[9]][["losses"]]
        data.table(team = team, id = id, wins = wins, losses = losses,
                   last10 = paste0(w10, "-", l10))
      }, simplify = FALSE) |> rbindlist()
    }, simplify = FALSE) |> rbindlist()
  }) |> rbindlist()
}

getStandings <- function(season) {
  standings <- .getStandings(season)
  standings[, pct := (wins / (wins + losses)) |>
              format(digits = 3, nsmall = 3)][]
}

.gb <- function(dt) {
  div <- copy(dt)
  div <- dt[order(-frank(wins))]

  lw <- div[1, wins]
  fw <- div[2:nrow(div), wins]

  ll <- div[1, losses]
  fl <- div[2:nrow(div), losses]

  div[, gb := c("--", ((lw - ll) - (fw - fl)) / 2)][]
}
