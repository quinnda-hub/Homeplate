gameScore <- function(dt) {
  #' `innings_pitched` is a character vector separated by a period.
  #' Each number before the period denotes one inning pitched (three outs), and
  #' each number after denotes just a singular out.
  innings <- strsplit(dt$innings_pitched, ".", fixed = TRUE)
  #' Complete innings
  ci <- Map(\(x) x[[1]], innings) |> as.numeric()
  #' Partial innings
  pi <- Map(\(x) x[[1]], innings) |> as.numeric()

  ### Game score is calculated as follows: ---

  #' One point is added for each inning pitched
  outs_bonus <- dt$outs

  #' Two points are added for each out after the fourth inning
  fourth_bonus <- (ci - 4) * 2

  #' One point is added for each strikeout
  so_bonus <- dt$strike_outs

  #' Subtract two points for each hit allowed
  hit_penalty <- dt$hits * 2

  #' Subtract four points for each earned run
  er_penalty <- dt$earned_runs * 4

  #' Subtract two points for each unearned run
  run_penalty <- (dt$runs - dt$earned_runs) * 2

  #' Subract one point for each walk
  bb_penalty <- dt$base_on_balls

  50 + outs_bonus + fourth_bonus + so_bonus - hit_penalty - er_penalty -
    run_penalty - bb_penalty
}

convertInningsPitched <- function(ip) {
  complete <- substr(ip, 1, 1)
  partial <- substr(ip, 3, 3)
  partial <- fifelse(partial == "2", ".666666",
                     fifelse(partial == "1", ".333333", ".0"))

  paste0(complete, partial)
}

# These functions will calculate ERA and FIP on a game by game basis.
FIP <- function(stats) {
  guts <- retry(GUTS())
  dt <-
    merge(stats[, season := as.integer(season)], guts[, .(Season, cFIP)],
          by.x = "season",
          by.y = "Season",
          all.x = TRUE)
  dt[, fip := ((((13 * home_runs) + (
    3 * (base_on_balls + hit_by_pitch)
  ) - (2 * strike_outs)) / as.numeric(innings_pitched)) + cFIP) |> round(2)][]
}

ERA <- function(stats) {
  dt <- copy(stats)

  dt[, era := (9 * (as.numeric(earned_runs) / as.numeric(innings_pitched))) |>
       round(2)][]
}

# this function will get year by year stats for any number of players
.yearStats <- function(playerId, group = "hitting") {
  base <- "https://statsapi.mlb.com/api/"
  end <-
    paste0("v1/people/", "?personIds=", Reduce(paste0, paste0(playerId, ",")))
  end <-
    paste0(
      substr(end, 1, nchar(end) - 1),
      "&",
      "hydrate=stats(group=[",
      Reduce(paste0, paste0(group, ",")),
      "]",
      "type=[yearByYear,yearByYearAdvanced])"
    )

  player <- GET(paste0(base, end)) |> content()

  aux <- function(players) {
    player <- sapply(players[["stats"]],
                     \(i) {
                       sapply(i[["splits"]],
                              \(j) {
                                s <- j[["season"]]

                                id <- j[["player"]][["id"]]

                                n <- j[["player"]][["fullName"]]

                                t <-
                                  if (is.null(j[["team"]][["name"]])) {
                                    "Combined"
                                  } else {
                                    j[["team"]][["name"]]
                                  }

                                ti <-
                                  if (is.null(j[["team"]][["id"]])) {
                                    "Combined"
                                  } else {
                                    j[["team"]][["id"]]
                                  }

                                c(list(id = id),
                                  list(name = n),
                                  list(season = s),
                                  list(team = t),
                                  list(team_id = ti),
                                  j[["stat"]])
                              },
                              simplify = FALSE)
                     },
                     simplify = FALSE)

    sapply(player, rbindlist, fill = TRUE, simplify = FALSE)
  }

  players <- sapply(player[["people"]], aux, simplify = FALSE) |>
    sapply(as.data.table, simplify = FALSE) |>
    rbindlist(fill = TRUE)

  #' Sometimes multiple year-by-year types will have the same columns
  #' we can remove them here

  players <-
    players[, which(!duplicated(t(players))), by = id, with = FALSE]

  if (nrow(players) == 0) {
    data.table(season = NA)
  } else {
    players[]
  }
}

yearStats <- function(playerId, group = "hitting") {
  # We need to chunk API calls into groups of 500 so that the query is
  # successful
  players <-
    Map(.yearStats, chunk(playerId, 500), group = group) |>
    rbindlist(fill = TRUE) |> janitor::clean_names()

  # If the player has no hitting, pitching, or fielding data - skip them.
  if (is.na(players[1, season])) {
    players[]
  } else {
    for (col in names(players))
      set(
        players,
        i = which(players[[col]] %in% c(".---", "-.--")),
        j = col,
        value = NA
      )
    if (group %in% c("hitting", "pitching")) {
      for (col in c("avg", "obp", "slg"))
        set(players, j = col,
            value = as.numeric(players[[col]]))
    }
    if (group == "pitching") {
      players <- FIP(players)
    }
  }
  players[]
}

# this function will get the game log for a player
.gameLogs <- function(playerId, season, group = "hitting") {
  base <- "https://statsapi.mlb.com/api/"
  end <-
    paste0("v1/people/", "?personIds=", Reduce(paste0, paste0(playerId, ",")))
  end <-
    paste0(
      substr(end, 1, nchar(end) - 1),
      "&",
      "hydrate=stats(group=[",
      Reduce(paste0, paste0(group, ",")),
      "]",
      "type=[gameLog],season=",
      season,
      ")"
    )

  players <- GET(paste0(base, end)) |> content()

  aux <- function(players) {
    player <- sapply(players[["stats"]][[1]][["splits"]],
                     \(i) {
                       n <- i[["player"]][["fullName"]]

                       d <- i[["date"]]

                       id <- i[["player"]][["id"]]

                       s <- i[["season"]]

                       t <- i[["team"]][["name"]]

                       ti <- i[["team"]][["id"]]
                       o <- i[["opponent"]][["name"]]

                       oi <- i[["opponent"]][["id"]]

                       g <- i[["game"]][["gamePk"]]

                       h <- i[["isHome"]]

                       w <- i[["isWin"]]

                       c(
                         list(game_pk = g),
                         list(date = d),
                         list(player_id = id),
                         list(name = n),
                         list(team = t),
                         list(team_id = ti),
                         list(season = s),
                         list(home = h),
                         list(win = w),
                         list(opponent = o),
                         list(opponent_id = oi),
                         i[["stat"]]
                       )
                     },
                     simplify = FALSE) |> rbindlist()
  }

  sapply(players[["people"]], aux, simplify = FALSE) |> rbindlist()
}

gameLogs <- function(playerId, season, group = "hitting") {
  logs <-
    Map(.gameLogs,
        chunk(playerId, 500),
        season = season,
        group = group) |>
    rbindlist(fill = TRUE) |> janitor::clean_names()

  logs[, date := as.Date(date)]

  for (col in names(logs))
    set(logs,
        i = which(logs[[col]] %in% c(".---", "-.--")),
        j = col,
        value = NA)

  if (group == "pitching") {
    logs[, `:=` (game_score = gameScore(logs),
                 innings_pitched = convertInningsPitched(innings_pitched))]
    logs <- FIP(logs)
  }

  logs[date < Sys.Date()]
}
