# this function will retrieve a data.table of gamePks for a given season
getGamePks <- function(season, gameType = "R") {
  start <- paste0("01/01/", season)
  end <- paste0("12/31/", season)

  base <- "https://statsapi.mlb.com/api/"
  end <-
    paste0(
      "v1/schedule?sportId=1&startDate=",
      start,
      "&endDate=",
      end,
      "&fields=dates,date,games,gamePk",
      "&gameTypes=",
      gameType
    )

  game_pks <- GET(paste0(base, end)) |> content()
  game_pks <- game_pks[["dates"]] |> rbindlist()
  game_pks[, date := as.Date(date)][, games := rbindlist(games)][]
}

# this function will generate the home and away team for a given game_pk
.getSchedule <- function(gamePks, sports_id = 1) {
  base <- "https://statsapi.mlb.com/api/"
  end <-
    paste0("v1/schedule/",
           "?sportsId=",
           sports_id,
           "&gamePks=",
           Reduce(paste0, paste0(gamePks, ",")))

  schedule <- GET(paste0(base, end)) |> content()

  aux <- function(games) {
    date <- games[["date"]]

    sapply(games[["games"]],
           \(i) {
             time <- i[["gameDate"]]
             game_pk <- i[["gamePk"]]

             sapply(seq_along(i[["teams"]]),
                    \(j) {
                      id <- i[["teams"]][[j]][["team"]][["id"]]

                      team <- i[["teams"]][[j]][["team"]][["name"]]

                      data.table(
                        date = date,
                        date_time = time,
                        game_pk = game_pk,
                        team_id = id,
                        team = team,
                        home = if (j == 1) {
                          FALSE
                        } else {
                          TRUE
                        },
                        away = if (j == 2) {
                          FALSE
                        } else {
                          TRUE
                        }
                      )
                    }, simplify = FALSE)
           } |>
             rbindlist(),
           simplify = FALSE) |> rbindlist()
  }

  sapply(schedule[["dates"]], aux, simplify = FALSE) |> rbindlist()
}

getSchedule <- function(gamePks, sports_id = 1) {
  format_time <- function(x) {
    x <- gsub("T", " ", x)
    x <- gsub("Z", "UTC", x)
    as.POSIXct(x)
  }

  schedule <-
    Map(.getSchedule, chunk(gamePks, 500), sports_id = sports_id) |>
    rbindlist(fill = TRUE) |> janitor::clean_names()
  schedule[, `:=` (date = as.Date(date),
                   date_time = format_time(date_time))][]
}
