labeledInput <- function(id, label, input) {
  div(id = id, span(label, style = "font-size: small;"), input)
}

kpi_mini <- function(label, value) {
  tags$div(
    style = "
      border:1px solid #e6e6e6; border-radius:12px; padding:8px 10px;
      background:#fff; box-shadow:0 1px 2px rgba(0,0,0,0.04);",
    tags$div(
      style = "color:#5f6368; font-size:11px; margin-bottom:4px;",
      label
    ),
    tags$div(
      style = "font-size:16px; font-weight:750; font-variant-numeric: tabular-nums;",
      value
    )
  )
}

battingLogsRctbl <- function(logs) {
  # Abbreviate team names
  abr <- teamIds() |>
    as.data.table() |>
    melt(
      measure.vars = names(teamIds() |> as.data.table()),
      variable.name = "team",
      value.name = "team_id"
    )

  dt <-
    logs[, .(
      home,
      hit_by_pitch,
      sac_flies,
      total_bases,
      date,
      team_id,
      opponent_id,
      win,
      plate_appearances,
      at_bats,
      runs,
      hits,
      doubles,
      triples,
      home_runs,
      rbi,
      base_on_balls,
      strike_outs,
      intentional_walks,
      stolen_bases,
      caught_stealing,
      left_on_base,
      avg,
      obp,
      slg,
      ops
    )][, win := as.character(win)]

  # Stats API provides avg, obp, slg, and ops cumulatively. I want them on a
  # game by game basis.
  dt[, `:=`(
    avg = hits / at_bats,
    obp = (hits + base_on_balls + hit_by_pitch) /
      (at_bats + base_on_balls + hit_by_pitch + sac_flies),
    slg = total_bases / at_bats
  )][, ops := obp + slg]

  # Pretty printing
  for (col in c("avg", "obp", "slg", "ops")) {
    set(dt, j = col, value = format(dt[[col]], digits = 3, nsmall = 3))
  }

  setorder(dt, -date)

  reactable::reactable(
    dt,
    columns = list(
      date = colDef(
        name = "Date",
        align = "left",
        minWidth = 100
      ),
      team_id = colDef(
        cell = \(value) {
          abr[team_id == value, team]
        },
        name = "Team",
        minWidth = 60
      ),
      opponent_id = colDef(
        cell = \(value, index) {
          val <- abr[team_id == value, team]
          if (dt[index, home]) {
            val
          } else {
            paste0("@", val)
          }
        },
        name = "Opponent",
        minWidth = 90
      ),
      win = colDef(
        name = "Result",
        cell = function(value) {
          if (value) {
            "Win"
          } else {
            "Loss"
          }
        },
        minWidth = 70
      ),
      plate_appearances = colDef(name = "PA"),
      at_bats = colDef(show = FALSE),
      runs = colDef(name = "R"),
      hits = colDef(name = "H"),
      doubles = colDef(name = "2B"),
      triples = colDef(name = "3B"),
      home_runs = colDef(name = "HR"),
      rbi = colDef(name = "RBI"),
      base_on_balls = colDef(name = "BB"),
      strike_outs = colDef(name = "SO"),
      intentional_walks = colDef(name = "IBB"),
      stolen_bases = colDef(name = "SB"),
      caught_stealing = colDef(name = "CS"),
      left_on_base = colDef(name = "LOB", minWidth = 45),
      hit_by_pitch = colDef(show = FALSE),
      sac_flies = colDef(show = FALSE),
      total_bases = colDef(show = FALSE),
      home = colDef(show = FALSE),
      avg = colDef(name = "AVG", minWidth = 55),
      obp = colDef(name = "OBP", minWidth = 55),
      slg = colDef(name = "SLG", minWidth = 55),
      ops = colDef(name = "OPS", minWidth = 55)
    ),
    defaultColDef = colDef(
      align = "center",
      na = "0",
      minWidth = 40
    ),
    compact = TRUE,
    style = list(
      fontFamily = gt::google_font("Fira Mono"),
      width = "100%",
      maxWidth = "none",
      height = "100%"
    ),
    pagination = FALSE
  )
}

battingStatsRctbl <- function(stats) {
  abr <- teamIds() |>
    as.data.table() |>
    melt(
      measure.vars = names(teamIds() |> as.data.table()),
      variable.name = "team",
      value.name = "team_id"
    )

  dt <- copy(stats)

  dt[, def := fielding + positional]

  dt <-
    dt[, .(
      season,
      team_id,
      games_played,
      plate_appearances,
      runs,
      hits,
      doubles,
      triples,
      home_runs,
      rbi,
      base_on_balls,
      strike_outs,
      stolen_bases,
      caught_stealing,
      avg,
      obp,
      slg,
      ops,
      woba,
      w_rc_plus,
      base_running,
      def,
      war
    )]

  dt[, w_rc_plus := format(w_rc_plus, digits = 1)]

  # Pretty printing
  for (col in c("avg", "obp", "slg", "ops", "woba")) {
    set(
      dt,
      j = col,
      value = scales::label_number(accuracy = 0.001)(as.numeric(dt[[col]]))
    )
  }
  for (col in c("base_running", "def", "war")) {
    set(
      dt,
      j = col,
      value = scales::label_number(accuracy = 0.1)(as.numeric(dt[[col]]))
    )
  }

  reactable::reactable(
    dt,
    columns = list(
      season = colDef(
        name = "Season",
        minWidth = 55,
        align = "left"
      ),
      team_id = colDef(
        name = "Team",
        minWidth = 65,
        cell = \(value) {
          if (value == "Combined") {
            "Combined"
          } else {
            abr[team_id == value, team]
          }
        }
      ),
      games_played = colDef(name = "G"),
      plate_appearances = colDef(name = "PA"),
      runs = colDef(name = "R"),
      hits = colDef(name = "H"),
      doubles = colDef(name = "2B"),
      triples = colDef(name = "3B"),
      home_runs = colDef(name = "HR"),
      rbi = colDef(name = "RBI"),
      base_on_balls = colDef(name = "BB"),
      strike_outs = colDef(name = "SO"),
      stolen_bases = colDef(name = "SB"),
      caught_stealing = colDef(name = "CS"),
      avg = colDef(name = "AVG", minWidth = 55),
      obp = colDef(name = "OBP", minWidth = 55),
      slg = colDef(name = "SLG", minWidth = 55),
      ops = colDef(name = "OPS", minWidth = 55),
      woba = colDef(name = "wOBA", minWidth = 55),
      w_rc_plus = colDef(name = "wRC+", minWidth = 55),
      base_running = colDef(name = "BsR", minWidth = 55),
      def = colDef(name = "Def", minWidth = 55),
      war = colDef(name = "WAR", minWidth = 55)
    ),
    compact = TRUE,
    style = list(
      fontFamily = gt::google_font("Fira Mono"),
      maxWidth = "none",
      height = "100%",
      width = "100%"
    ),
    pagination = FALSE,
    defaultColDef = colDef(
      minWidth = 40,
      align = "center",
      na = "0"
    )
  )
}

pitchingLogsRctbl <- function(logs) {
  abr <- teamIds() |>
    as.data.table() |>
    melt(
      measure.vars = names(teamIds() |> as.data.table()),
      variable.name = "team",
      value.name = "team_id"
    )

  dt <-
    logs[, .(
      date,
      home,
      team_id,
      opponent_id,
      games_started,
      wins,
      losses,
      saves,
      holds,
      innings_pitched,
      batters_faced,
      hits,
      runs,
      earned_runs,
      home_runs,
      base_on_balls,
      strike_outs,
      walks_per9inn,
      strikeouts_per9inn,
      home_runs_per9,
      era,
      fip,
      game_score
    )] |>
    ERA()

  # Pretty printing
  for (col in c("era", "fip")) {
    set(dt, j = col, value = format(dt[[col]], digits = 2, nsmall = 2))
  }

  setorder(dt, -date)

  reactable::reactable(
    dt,
    columns = list(
      date = colDef(
        name = "Date",
        align = "left",
        minWidth = 100
      ),
      home = colDef(show = FALSE),
      team_id = colDef(
        name = "Team",
        cell = \(value) {
          abr[team_id == value, team]
        },
        minWidth = 60
      ),
      opponent_id = colDef(
        name = "Opponent",
        cell = \(value, index) {
          val <- abr[team_id == value, team]
          if (dt[index, home]) {
            val
          } else {
            paste0("@", val)
          }
        },
        minWidth = 90
      ),
      games_started = colDef(name = "GS"),
      wins = colDef(name = "W"),
      losses = colDef(name = "L"),
      saves = colDef(name = "SV"),
      holds = colDef(name = "HLD", minWidth = 45),
      innings_pitched = colDef(name = "IP", cell = function(value) {
        complete <- substr(value, 1, 1)
        partial <- substr(value, 3, 3)
        partial <- fifelse(
          partial == "6",
          ".2",
          fifelse(partial == "3", ".1", ".0")
        )

        paste0(complete, partial)
      }),
      batters_faced = colDef(name = "TBF"),
      hits = colDef(name = "H"),
      runs = colDef(name = "R"),
      earned_runs = colDef(name = "ER"),
      home_runs = colDef(name = "HR"),
      base_on_balls = colDef(name = "BB"),
      strike_outs = colDef(name = "SO"),
      walks_per9inn = colDef(name = "BB/9", minWidth = 45),
      strikeouts_per9inn = colDef(name = "SO/9", minWidth = 45),
      home_runs_per9 = colDef(name = "HR/9", minWidth = 45),
      era = colDef(name = "ERA", minWidth = 40),
      fip = colDef(name = "FIP"),
      game_score = colDef(name = "GSc")
    ),
    defaultColDef = colDef(
      align = "center",
      na = "0",
      minWidth = 35
    ),
    compact = TRUE,
    pagination = FALSE,
    style = list(
      fontFamily = gt::google_font("Fira Mono"),
      width = "100%",
      maxWidth = "none",
      height = "100%"
    )
  )
}

pitchingStatsRctbl <- function(stats) {
  abr <- teamIds() |>
    as.data.table() |>
    melt(
      measure.vars = names(teamIds() |> as.data.table()),
      variable.name = "team",
      value.name = "team_id"
    )

  dt <- copy(stats)
  if (is.null(dt$games_started)) {
    dt[, games_started := dt$games_played]
  }
  if (is.null(dt$saves)) {
    dt[, saves := 0]
  }

  dt <-
    dt[, .(
      season,
      team_id,
      games_played,
      games_started,
      wins,
      losses,
      saves,
      innings_pitched,
      walks_per9inn,
      strikeouts_per9inn,
      home_runs_per9,
      home_runs_per_plate_appearance,
      babip,
      era,
      i.fip,
      xfip,
      era_minus,
      war
    )]

  # Pretty printing
  for (col in c("era", "i.fip", "xfip")) {
    set(
      dt,
      j = col,
      value = scales::label_number(accuracy = 0.01)(as.numeric(dt[[col]]))
    )
  }

  dt[, era_minus := scales::label_number(accuracy = 3)(as.numeric(era_minus))]
  dt[, war := scales::label_number(accuracy = 0.1)(as.numeric(war))]

  reactable::reactable(
    dt,
    columns = list(
      season = colDef(name = "Season", minWidth = 55),
      team_id = colDef(
        name = "Team",
        cell = \(value) {
          if (value == "Combined") {
            "Combined"
          } else {
            abr[team_id == value, team]
          }
        },
        minWidth = 65
      ),
      games_played = colDef(name = "G"),
      games_started = colDef(name = "GS"),
      wins = colDef(name = "W"),
      losses = colDef(name = "L"),
      saves = colDef(name = "SV"),
      innings_pitched = colDef(name = "IP"),
      walks_per9inn = colDef(name = "BB/9"),
      strikeouts_per9inn = colDef(name = "SO/9"),
      home_runs_per9 = colDef(name = "HR/9"),
      home_runs_per_plate_appearance = colDef(
        name = "HR%",
        cell = function(value) {
          as.numeric(value) * 100 |> round(2)
        }
      ),
      babip = colDef(name = "BABIP"),
      era = colDef(name = "ERA"),
      i.fip = colDef(name = "FIP"),
      xfip = colDef(name = "xFIP"),
      era_minus = colDef(name = "ERA-"),
      war = colDef(name = "WAR")
    ),
    defaultColDef = colDef(
      align = "center",
      na = "0",
      minWidth = 40
    ),
    compact = TRUE,
    pagination = FALSE,
    style = list(
      fontFamily = gt::google_font("Fira Mono"),
      width = "100%",
      maxWidth = "none",
      height = "100%"
    )
  )
}

standingsRctbl <- function(dt, division) {
  if (division == "al west") {
    dt <- dt[id %in% c(117, 108, 140, 136, 133)]
    name <- "AL West"
  } else if (division == "al central") {
    dt <- dt[id %in% c(142, 145, 114, 118, 116)]
    name <- "AL Central"
  } else if (division == "al east") {
    dt <- dt[id %in% c(147, 139, 141, 111, 110)]
    name <- "AL East"
  } else if (division == "nl west") {
    dt <- dt[id %in% c(119, 135, 137, 109, 115)]
    name <- "NL West"
  } else if (division == "nl central") {
    dt <- dt[id %in% c(158, 138, 134, 112, 113)]
    name <- "NL Central"
  } else if (division == "nl east") {
    dt <- dt[id %in% c(121, 144, 143, 146, 120)]
    name <- "NL East"
  } else {
    stop("Please input valid division")
  }

  standings <- dt
  setcolorder(
    standings,
    c("team", "id", "wins", "losses", "pct", "gb", "last10")
  )

  reactable(
    standings,
    columns = list(
      team = colDef(
        cell = function(value) {
          image <- img(
            src = paste0("logos/", dt[team == value, id], ".svg"),
            height = "24px",
            alt = value
          )
          tagList(
            div(
              style = list(
                display = "inline-block",
                width = "45px"
              ),
              image
            ),
            value
          )
        },
        name = name,
        minWidth = 135,
        align = "left"
      ),
      id = colDef(show = FALSE),
      wins = colDef(name = "Wins", align = "center", minWidth = 50),
      losses = colDef(name = "Losses", align = "center", minWidth = 50),
      pct = colDef(name = "PCT", align = "center", minWidth = 50),
      gb = colDef(name = "GB", align = "center", minWidth = 50),
      last10 = colDef(name = "L10", align = "center", minWidth = 50)
    ),
    compact = FALSE,
    style = list(
      fontFamily = gt::google_font("Fira Mono"),
      width = "100%",
      maxWidth = "none",
      height = "100%"
    ),
    sortable = FALSE
  )
}

formatPlayerInfo <- function(dtPlayerInfo) {
  dt <- dtPlayerInfo

  p(
    strong(dt$full_name, paste0("#", dt$primary_number)),
    br(),
    strong(
      dt$primary_position,
      "|",
      "B/T:",
      paste0(substr(dt[, bat_side], 1, 1), "/", substr(dt[, pitch_hand], 1, 1)),
      br(),
      dt$height,
      "/",
      dt$weight,
      "|",
      "Age:",
      dt$current_age
    ),
    br(),
    strong("Born:"),
    dt$birth_date,
    "in",
    paste0(dt$birth_city, ","),
    dt$birth_country
  )
}

# A plot of a players avg, obp, slg, or ops.
# n = the rolling window size.
battingLogsPlot <- function(logs, stat, n) {
  dt <- copy(logs)

  # Fast way to average out the columns we need.
  cols <- c(
    "hits",
    "at_bats",
    "base_on_balls",
    "hit_by_pitch",
    "sac_flies",
    "total_bases"
  )

  for (j in cols) {
    set(dt, j = j, value = dt[[j]] |> frollmean(n) |> round(2))
  }
  # Stats API provides avg, obp, slg, and ops cumulatively. I want them on a
  # game by game basis.
  dt[, `:=`(
    avg = hits / at_bats,
    obp = (hits + base_on_balls + hit_by_pitch) /
      (at_bats + base_on_balls + hit_by_pitch + sac_flies),
    slg = total_bases / at_bats
  )][, ops := (obp + slg) |> round(3)]

  fig <- plot_ly()

  fig <- fig |>
    add_trace(
      data = dt,
      x = ~date,
      y = ~ get(stat),
      fill = "tozeroy",
      fillcolor = "rgba(255, 212, 96, 0.5)",
      color = I("#FFBA00"),
      type = "scatter",
      mode = "lines",
      line = list(width = 0.5)
    ) |>
    layout(
      yaxis = list(title = NA, tickformat = ".3f"),
      xaxis = list(title = NA),
      hovermode = "x"
    ) |>
    config(
      modeBarButtonsToRemove = c(
        "zoom",
        "pan",
        "scale",
        "zoomIn",
        "zoomOut",
        "autoScale",
        "resetScale",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      ),
      toImageButtonOptions = list(
        format = "png",
        height = 1080,
        width = 1920,
        scale = 3,
        filename = head(dt$name, 1)
      ),
      displaylogo = FALSE
    )

  fig
}

# A plot of a players innings pitched, era, fip, or game score.
# n = the rolling window size.
pitchingLogsPlot <- function(logs, stat, n) {
  dt <- copy(logs) |> ERA()
  dt[, `:=`(
    innings_pitched = as.numeric(innings_pitched),
    era = as.numeric(era)
  )]

  # Fast way to average out the columns we need.
  cols <- c("innings_pitched", "era", "fip", "game_score")

  tck <- if (stat %in% c("era", "fip")) {
    ".2f"
  } else {
    ",d"
  }

  for (j in cols) {
    set(dt, j = j, value = dt[[j]] |> frollmean(n) |> round(2))
  }

  fig <- plot_ly()

  fig <- fig |>
    add_trace(
      data = dt,
      x = ~date,
      y = ~ get(stat),
      fill = "tozeroy",
      fillcolor = "rgba(255, 212, 96, 0.5)",
      color = I("#FFBA00"),
      type = "scatter",
      mode = "lines",
      line = list(width = 0.5)
    ) |>
    layout(
      yaxis = list(title = NA, tickformat = tck),
      xaxis = list(title = NA),
      hovermode = "x"
    ) |>
    config(
      modeBarButtonsToRemove = c(
        "zoom",
        "pan",
        "scale",
        "zoomIn",
        "zoomOut",
        "autoScale",
        "resetScale",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      ),
      toImageButtonOptions = list(
        format = "png",
        height = 1080,
        width = 1920,
        scale = 3,
        filename = head(dt$name, 1)
      ),
      displaylogo = FALSE
    )

  fig
}
