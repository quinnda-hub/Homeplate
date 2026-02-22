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
  guts <- GUTS_cache()
  dt <-
    merge(stats[, season := as.integer(season)], guts[, .(season, cFIP)],
          by.x = "season",
          by.y = "season",
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

  sapply(players[["people"]], aux, simplify = FALSE) |> rbindlist(fill = TRUE)
}

gameLogs <- function(playerId, season, group = "hitting") {
  logs <-
    Map(.gameLogs,
        chunk(playerId, 500),
        season = season,
        group = group) |>
    rbindlist(fill = TRUE) |> janitor::clean_names()

  logs[, "date" := as.Date(date)]

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

playerSabermetrics <- function(playerId,
                               season,
                               group = c("hitting", "pitching"),
                               sportId = 1,
                               gameType = "R") {
  stopifnot(length(playerId) == 1)

  group <- match.arg(group, several.ok = TRUE)

  fetch_one <- function(g) {
    url <- sprintf("https://statsapi.mlb.com/api/v1/people/%s/stats", playerId)

    resp <- httr::GET(
      url,
      query = list(
        stats = "sabermetrics",
        group = g,
        season = season,
        sportId = sportId,
        gameType = gameType
      )
    )

    httr::stop_for_status(resp)
    x <- httr::content(resp, as = "parsed", type = "application/json")

    stats_list <- x$stats %||% list()
    if (!length(stats_list)) {
      return(data.table::data.table())
    }

    splits <- stats_list[[1]]$splits %||% list()
    if (!length(splits)) {
      return(data.table::data.table())
    }

    rows <- lapply(splits, function(s) {
      meta <- list(
        player_id   = s$player$id %||% playerId,
        player_name = s$player$fullName %||% NA_character_,
        season      = s$season %||% as.character(season),
        group       = g,
        team_id     = s$team$id %||% NA_integer_,
        team        = s$team$name %||% NA_character_,
        league_id   = s$league$id %||% NA_integer_,
        league      = s$league$name %||% NA_character_,
        game_type   = s$gameType %||% NA_character_
      )

      data.table::as.data.table(c(meta, s$stat))
    })

    dt <- data.table::rbindlist(rows, fill = TRUE)
    dt <- janitor::clean_names(dt)

    keep_chr <- c("player_name", "season", "group", "team", "league", "game_type")
    num_cols <- setdiff(names(dt), keep_chr)
    for (col in num_cols) {
      if (is.character(dt[[col]])) {
        suppressWarnings(data.table::set(dt, j = col, value = as.numeric(dt[[col]])))
      }
    }

    dt[]
  }

  out <- data.table::rbindlist(lapply(group, fetch_one), fill = TRUE)
  out[]
}

