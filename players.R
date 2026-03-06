# this function will get the player_id for each player on a team's roster
.getRoster <- function(teamId, roster = "40Man") {
  base <- "https://statsapi.mlb.com/api/"
  end <-
    paste0("v1/teams/", teamId, "/roster?rosterType=", roster)

  team <- GET(paste0(base, end)) |> content()

  # Expected MLB Stats API roster fields: person(id, fullName, jerseyNumber),
  # parentTeamId, position(name), and status(description).
  if (length(team[["roster"]]) == 0) {
    return(data.table(
      player_id = integer(),
      team_id = integer(),
      name = character(),
      jersey = character(),
      position = character(),
      status = character()
    ))
  }

  lapply(team[["roster"]], \(player) {
    data.table(
      player_id = player[["person"]][["id"]],
      team_id = player[["parentTeamId"]],
      name = player[["person"]][["fullName"]],
      jersey = player[["person"]][["jerseyNumber"]],
      position = player[["position"]][["name"]],
      status = player[["status"]][["description"]]
    )
  }) |>
    rbindlist(fill = TRUE)
}

getRoster <- function(teamId, roster = "40Man") {
  players <- Map(.getRoster, teamId, roster = roster) |> rbindlist(fill = TRUE)
  players[!duplicated(player_id)][]
}

# this function will retrieve general info on a player
.playerInfo <- function(playerId) {
  base <- "https://statsapi.mlb.com/api/"
  end <- paste0("v1/people/", "?personIds=")

  player <-
    GET(Reduce(paste0, paste0(playerId, ","), paste0(base, end))) |> content()

  player <-
    sapply(
      seq_along(player[["people"]]),
      \(x) {
        player[["people"]][[x]] |>
          as.data.table()
      },
      simplify = FALSE
    ) |>
    rbindlist(fill = TRUE)

  player[,
    `:=`(
      primaryPosition = primaryPosition[[2]],
      batSide = batSide[[2]],
      pitchHand = pitchHand[[2]]
    ),
    by = id
  ][!duplicated(id), ]
}

playerInfo <- function(playerId) {
  info <-
    Map(.playerInfo, chunk(playerId, 500)) |> rbindlist(fill = TRUE)

  info[, `:=`(
    primaryPosition = unlist(primaryPosition),
    batSide = unlist(batSide),
    pitchHand = unlist(pitchHand),
    birthDate = as.Date(birthDate),
    mlbDebutDate = as.Date(mlbDebutDate)
  )] |>
    janitor::clean_names()
}

# this function will retrieve information on all free agents declared for a
# given season
.freeAgents <- function(season) {
  base <- "https://statsapi.mlb.com/api/"
  end <- paste0("v1/people/freeAgents?season=", season)

  fas <- GET(paste0(base, end)) |> content()

  sapply(
    fas[["freeAgents"]],
    \(i) {
      n <- i[["player"]][["fullName"]]

      e <- i[["player"]][["id"]]

      p <- i[["position"]][["abbreviation"]]

      o <- i[["originalTeam"]][["name"]]

      c <- i[["newTeam"]][["name"]]

      d <- i[["dateDeclared"]]

      s <- i[["dateSigned"]]

      g <- i[["notes"]]

      data.table(
        id = e,
        name = n,
        position = p,
        original_team = o,
        new_team = c,
        date_declared = d,
        date_signed = s,
        notes = g
      )
    },
    simplify = FALSE
  ) |>
    rbindlist(fill = TRUE)
}

freeAgents <- function(season) {
  fas <- .freeAgents(season)
  fas[, `:=`(
    date_declared = as.Date(date_declared),
    date_signed = as.Date(date_signed)
  )]
  fas
}
