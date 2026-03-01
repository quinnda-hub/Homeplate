# Create cache for player head shots.
dir.create("www/players", FALSE)
temp_dir <- "www/players"

cf <- cache_disk(max_age = 24 * 60 * 60)
dh <- memoise(downloadHeadshot, cache = cf)
sc_contact_cache <- cache_disk(max_age = 6 * 60 * 60)
sc_contact <- memoise(statcast_contact, cache = sc_contact_cache)
sc_pitch_cache <- cache_disk(max_age = 6 * 60 * 60)
sc_pitch <- memoise(statcast_pitch, cache = sc_pitch_cache)

# Contains all the values that should be shared across sessions.
global_vals <-
  reactiveValues(
    standings = getStandings(getCurrentSeason()),
    active_players = reactiveFileReader(
      6e4,
      session = NULL,
      filePath = "www/data/active_players.rds",
      readFunc = readRDS
    ),
    player_info = reactiveFileReader(
      6e4,
      session = NULL,
      filePath = "www/data/player_info.rds",
      readFunc = readRDS
    ),
    batting_logs = reactiveFileReader(
      6e4,
      session = NULL,
      filePath = "www/data/batting_logs.rds",
      readFunc = readRDS
    ),
    batting_stats = reactiveFileReader(
      6e4,
      session = NULL,
      filePath = "www/data/batting_stats.rds",
      readFunc = readRDS
    ),
    pitching_logs = reactiveFileReader(
      6e4,
      session = NULL,
      filePath = "www/data/pitching_logs.rds",
      readFunc = readRDS
    ),
    pitching_stats = reactiveFileReader(
      6e4,
      session = NULL,
      filePath = "www/data/pitching_stats.rds",
      readFunc = readRDS
    )
  )

# Update the standings.
observe({
  invalidateLater(3e5, session = NULL)
  global_vals$standings <- getStandings(getCurrentSeason())
})

server <- function(input, output, session) {
  # Update the player search.
  observe({
    updateSelectizeInput(
      inputId = "player_search",
      server = TRUE,
      choices = global_vals$active_players() |> searchList()
    )
  })

  # Update the stat search.
  observe({
    req(input$player_search)

    if (
      identical(input$stats, "Contact Lab") ||
        identical(input$stats, "Pitching Lab")
    ) {
      return()
    }

    choices <- if (input$stats == "Pitching") {
      c("ERA", "FIP", "Game Score", "Innings Pitched")
    } else {
      c("AVG", "OBP", "SLG", "OPS")
    }

    updateSelectizeInput(
      session,
      "stat_search",
      choices = choices,
      server = FALSE
    )

    val <- if (input$stats == "Pitching") 3 else 6
    max <- if (input$stats == "Pitching") 14 else 30
    updateSliderInput(session, "rolling_window", value = val, max = max)
  })

  # Six standings tables for each division.
  output$al_west <-
    renderReactable(standingsRctbl(global_vals$standings, "al west"))
  output$al_central <-
    renderReactable(standingsRctbl(global_vals$standings, "al central"))
  output$al_east <-
    renderReactable(standingsRctbl(global_vals$standings, "al east"))
  output$nl_west <-
    renderReactable(standingsRctbl(global_vals$standings, "nl west"))
  output$nl_central <-
    renderReactable(standingsRctbl(global_vals$standings, "nl central"))
  output$nl_east <-
    renderReactable(standingsRctbl(global_vals$standings, "nl east"))

  # Tables for player stats.
  output$batting_logs <- renderReactable({
    req(input$player_search)
    blogs <- global_vals$batting_logs()
    battingLogsRctbl(blogs[player_id == input$player_search])
  })

  output$batting_stats <- renderReactable({
    req(input$player_search)
    bstats <- global_vals$batting_stats()
    battingStatsRctbl(bstats[id == input$player_search])
  })

  output$pitching_logs <- renderReactable({
    req(input$player_search)
    plogs <- global_vals$pitching_logs()
    pitchingLogsRctbl(plogs[player_id == input$player_search])
  })

  output$pitching_stats <- renderReactable({
    req(input$player_search)
    pstats <- global_vals$pitching_stats()
    pitchingStatsRctbl(pstats[id == input$player_search])
  })

  # Since head shots can change, especially around the trade deadline, we want
  # to make sure that we download recent photos, but without downloading the same
  # photo every time. Hence, the images are stored in a 24 hour cache.
  output$head_shot <- renderImage(
    {
      req(input$player_search)

      path <- paste0("www/players", "/", input$player_search, ".png")

      # Cached version of downloadHeadshot().
      dh(input$player_search, path)

      list(
        src = paste0(
          "www/players",
          "/",
          input$player_search,
          ".png"
        ),
        contentType = "image/png",
        width = 106,
        height = 160,
        alt = global_vals$active_players()[
          player_id == input$player_search,
          name
        ]
      )
    },
    deleteFile = FALSE
  )

  # Some summary information on the selected player.
  output$summary <- renderUI({
    req(input$player_search)

    player <- global_vals$player_info()[id == input$player_search]

    formatPlayerInfo(player)
  })

  output$plot <- renderPlotly({
    req(input$player_search, input$stat_search)

    stat <- if (input$stat_search == "Innings Pitched") {
      "innings_pitched"
    } else if (input$stat_search == "Game Score") {
      "game_score"
    } else {
      tolower(input$stat_search)
    }

    if (input$stats == "Pitching") {
      plogs <- global_vals$pitching_logs()
      pitchingLogsPlot(
        plogs[player_id == input$player_search],
        stat,
        input$rolling_window
      )
    } else {
      blogs <- global_vals$batting_logs()
      battingLogsPlot(
        blogs[player_id == input$player_search],
        stat,
        input$rolling_window
      )
    }
  })

  # If a player's primary position is a Pitcher, display pitching stats
  # automatically. Else, show their batting stats.
  # This also controls inputs for the plot under the head shot.
  observeEvent(input$player_search, {
    req(input$player_search)

    # If user is currently on Contact Lab, don't override their tab choice.
    if (
      identical(isolate(input$stats), "Contact Lab") ||
        identical(input$stats, "Pitching Lab")
    ) {
      return()
    }

    position <- global_vals$active_players()[
      player_id == input$player_search,
      position
    ]
    selection <- if (position == "Pitcher") "Pitching" else "Batting"

    updateTabsetPanel(session, "stats", selected = selection)
  })

  observeEvent(input$player_search, {
    if (input$player_search == "") {
      hide("controls")
    } else {
      delay(500, show("controls"))
    }
  })

  contact_data_raw <- reactive({
    req(input$stats == "Contact Lab", input$player_search, input$contact_dates)

    tryCatch(
      sc_contact(
        input$player_search,
        input$contact_dates[[1]],
        input$contact_dates[[2]]
      ),
      error = function(e) {
        data.table()
      }
    )
  })

  observeEvent(contact_data_raw(), {
    dt <- contact_data_raw()

    if (!nrow(dt)) {
      updateSelectizeInput(
        session,
        "contact_pitch_type",
        choices = "All",
        selected = "All"
      )
      updateSelectizeInput(
        session,
        "contact_bb_type",
        choices = "All",
        selected = "All"
      )
      return()
    }

    updateSelectizeInput(
      session,
      "contact_pitch_type",
      choices = c("All", sort(unique(stats::na.omit(dt$pitch_name)))),
      selected = "All"
    )

    updateSelectizeInput(
      session,
      "contact_bb_type",
      choices = c("All", sort(unique(dt$bb_type))),
      selected = "All"
    )
  })

  contact_data <- reactive({
    dt <- contact_data_raw()
    if (!nrow(dt)) {
      return(dt)
    }

    if (
      !is.null(input$contact_pitch_type) &&
        input$contact_pitch_type != "All"
    ) {
      dt <- dt[pitch_name == input$contact_pitch_type]
    }

    if (
      !is.null(input$contact_bb_type) &&
        input$contact_bb_type != "All"
    ) {
      dt <- dt[bb_type == input$contact_bb_type]
    }

    dt
  })

  output$contact_summary <- renderUI({
    dt <- contact_data()

    validate(
      need(nrow(dt) > 0, "No Statcast contact data available for this range.")
    )

    balls_in_play <- dt[type == "X"]

    validate(
      need(
        nrow(balls_in_play) > 0,
        "No batted-ball data for the selected filters."
      )
    )

    avg_ev <- mean(balls_in_play$launch_speed, na.rm = TRUE)
    avg_la <- mean(balls_in_play$launch_angle, na.rm = TRUE)
    p90_ev <- as.numeric(stats::quantile(
      balls_in_play$launch_speed,
      probs = 0.9,
      na.rm = TRUE
    ))

    hard_hit_rate <- mean(balls_in_play$launch_speed >= 95, na.rm = TRUE)
    sweet_spot_rate <- mean(
      balls_in_play$launch_angle >= 8 &
        balls_in_play$launch_angle <= 32,
      na.rm = TRUE
    )

    damage_zone_rate <- mean(
      balls_in_play$launch_speed >= 95 &
        balls_in_play$launch_angle >= 8 &
        balls_in_play$launch_angle <= 32,
      na.rm = TRUE
    )
    pop_rate <- mean(balls_in_play$launch_angle > 50, na.rm = TRUE)

    stat_row <- function(label, value) {
      tags$div(
        class = "sb-row",
        tags$div(class = "sb-label", label),
        tags$div(class = "sb-value", value)
      )
    }

    tags$div(
      style = "display:grid; grid-template-columns:1fr; gap:10px; margin-top:10px;",
      kpi_mini("Batted balls", nrow(balls_in_play)),
      kpi_mini("Avg EV / LA", sprintf("%.1f mph / %.1f°", avg_ev, avg_la)),
      kpi_mini("90th pct EV", sprintf("%.1f mph", p90_ev)),
      kpi_mini("Hard-hit", sprintf("%.1f%%", hard_hit_rate * 100)),
      kpi_mini("Sweet-spot", sprintf("%.1f%%", sweet_spot_rate * 100)),
      kpi_mini("Damage-zone", sprintf("%.1f%%", damage_zone_rate * 100)),
      kpi_mini("Pop-ups (>50°)", sprintf("%.1f%%", pop_rate * 100)),

      tags$details(
        style = "margin-top:6px;",
        tags$summary(
          style = "cursor:pointer; color:#5f6368; font-size:11px;",
          "Benchmarks"
        ),
        tags$div(
          class = "sb-note",
          "Hard-hit: 95+ mph. Sweet-spot: 8–32° LA. Damage zone: 95+ EV and 8-32° LA."
        )
      )
    )
  })

  output$contact_ev_density <- renderPlotly({
    dt <- contact_data()

    validate(
      need(nrow(dt) > 0, "No Statcast contact data available for this range.")
    )

    balls_in_play <- dt[type == "X"]

    validate(
      need(
        nrow(balls_in_play) > 0,
        "No batted-ball data for the selected filters."
      )
    )

    plotContactEVAngleDensity(balls_in_play)
  })

  output$contact_spray <- renderPlotly({
    dt <- contact_data()

    validate(
      need(nrow(dt) > 0, "No Statcast contact data available for this range.")
    )

    balls_in_play <- dt[type == "X"]

    validate(
      need(
        nrow(balls_in_play) > 0,
        "No spray chart data for the selected filters."
      )
    )

    plotContactSprayChart(balls_in_play)
  })

  pitchlab_data_raw <- reactive({
    req(
      input$stats == "Pitching Lab",
      input$player_search,
      input$pitchlab_dates
    )

    tryCatch(
      tibble::as.tibble(
        sc_pitch(
          input$player_search,
          input$pitchlab_dates[[1]],
          input$pitchlab_dates[[2]]
        )
      ),
      error = function(e) {
        tibble::tibble()
      }
    )
  })

  observeEvent(pitchlab_data_raw(), {
    tbl <- pitchlab_data_raw()

    if (!nrow(tbl)) {
      updateSelectInput(
        session,
        "pitchlab_pitch_type",
        choices = "All",
        selected = "All"
      )
      return()
    }

    updateSelectInput(
      session,
      "pitchlab_pitch_type",
      choices = c("All", sort(unique(stats::na.omit(tbl$pitch_name)))),
      selected = "All"
    )
  })

  pitchlab_data <- reactive({
    tbl <- pitchlab_data_raw()
    if (!nrow(tbl)) {
      return(tbl)
    }

    selected_types <- input$pitchlab_pitch_type
    if (
      !is.null(selected_types) &&
        length(selected_types) &&
        !("All" %in% selected_types)
    ) {
      tbl <- tbl |>
        dplyr::filter(pitch_name %in% selected_types)
    }

    count_states <- input$pitchlab_count_state
    if (
      !is.null(count_states) &&
        length(count_states) &&
        !("All" %in% count_states)
    ) {
      tbl <- tbl |>
        dplyr::mutate(
          count_state = dplyr::if_else(
            strikes > balls,
            "Ahead",
            dplyr::if_else(balls > strikes, "Behind", "Even")
          )
        )
      tbl <- tbl |>
        dplyr::filter(count_state %in% count_states)
    }

    tto <- input$pitchlab_tto
    if (!is.null(tto) && length(tto) && !("All" %in% tto)) {
      tbl <- tbl |>
        dplyr::mutate(
          tto_bucket = dplyr::if_else(
            is.na(n_thruorder_pitcher),
            NA_character_,
            dplyr::if_else(
              n_thruorder_pitcher >= 3,
              "3+",
              as.character(n_thruorder_pitcher)
            )
          )
        )
      tbl <- tbl |> dplyr::filter(tto_bucket %in% tto)
    }

    batter_hand <- input$pitchlab_batter_hand
    if (
      !is.null(batter_hand) && length(batter_hand) && !("All" %in% batter_hand)
    ) {
      tbl <- tbl |> dplyr::filter(stand %in% batter_hand)
    }

    zone_filter <- input$pitchlab_zone_filter
    if (
      !is.null(zone_filter) && length(zone_filter) && !("All" %in% zone_filter)
    ) {
      in_zone <- !is.na(tbl$zone) & tbl$zone %in% 1:9
      if ("In Zone" %in% zone_filter && !"Out of Zone" %in% zone_filter) {
        tbl <- tbl[in_zone, ]
      } else if (
        "Out of Zone" %in% zone_filter && !"In Zone" %in% zone_filter
      ) {
        tbl <- tbl[!in_zone, ]
      }
    }

    runners <- input$pitchlab_runners
    if (!is.null(runners) && length(runners) && !("All" %in% runners)) {
      tbl <- tbl |>
        dplyr::mutate(
          has_runners = !is.na(on_1b) | !is.na(on_2b) | !is.na(on_3b)
        )
      if ("Bases Empty" %in% runners && !"Runners On" %in% runners) {
        tbl <- tbl |> dplyr::filter(!has_runners)
      } else if ("Runners On" %in% runners && !"Bases Empty" %in% runners) {
        tbl <- tbl |> dplyr::filter(has_runners)
      }
    }

    if (isTRUE(input$pitchlab_two_strike)) {
      tbl <- tbl |> dplyr::filter(strikes == 2)
    }

    tbl
  })

  output$pitchlab_summary <- renderUI({
    tbl <- pitchlab_data()

    validate(need(
      nrow(tbl) > 0,
      "No Statcast pitching data available for this range."
    ))

    attack <- summarise_pitch_attack(tbl)

    pitch_lvl <- summarise_pitch_level(tbl)
    pa_end <- summarise_pa_ending(tbl)

    # aggregate pitch-level across pitch types (weighted by pitch_n)
    pitch_tot <- pitch_lvl |>
      dplyr::summarise(
        whiff_pct = sum(whiff_pct * pitch_n, na.rm = TRUE) /
          sum(pitch_n, na.rm = TRUE),
        putaway_pct = sum(putaway_pct * pitch_n, na.rm = TRUE) /
          sum(pitch_n, na.rm = TRUE)
      )

    # aggregate PA-ending across pitch types
    pa_tot <- pa_end |>
      dplyr::summarise(
        pa_n = sum(.data$pa, na.rm = TRUE),
        bbe_n = sum(.data$bbe, na.rm = TRUE),

        avg_ev = {
          den <- sum(.data$bbe, na.rm = TRUE)
          if (den == 0) {
            NA_real_
          } else {
            sum(.data$ev * .data$bbe, na.rm = TRUE) / den
          }
        },

        hardhit_pct = {
          den <- sum(.data$bbe, na.rm = TRUE)
          if (den == 0) {
            NA_real_
          } else {
            sum(.data$hardhit * .data$bbe, na.rm = TRUE) / den
          }
        }
      )

    # K% and BB% need PA-level denominators.
    # We already have so in pa_end; for BB we can infer from pa_end events by reusing a small calc:
    by_pa_last <- tbl |>
      tibble::as_tibble() |>
      dplyr::filter(
        !is.na(game_pk),
        !is.na(at_bat_number),
        !is.na(pitch_number)
      ) |>
      dplyr::group_by(game_pk, at_bat_number) |>
      dplyr::slice_max(pitch_number, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::filter(
        (!is.na(events) & events != "") | (!is.na(type) & type == "X")
      )

    total_pas <- nrow(by_pa_last)

    is_so <- !is.na(by_pa_last$events) &
      by_pa_last$events %in% c("strikeout", "strikeout_double_play")
    is_bb <- !is.na(by_pa_last$events) &
      by_pa_last$events %in% c("walk", "intent_walk")

    k_pct <- if (total_pas == 0) NA_real_ else mean(is_so, na.rm = TRUE)
    bb_pct <- if (total_pas == 0) NA_real_ else mean(is_bb, na.rm = TRUE)

    # Values
    total_pitches <- attack$pitches
    zone_pct <- attack$zone_pct
    csw_pct <- attack$csw_pct
    fps_pct <- attack$fps_pct
    whiff_pct <- pitch_tot$whiff_pct
    putaway_pct <- pitch_tot$putaway_pct

    avg_ev <- pa_tot$avg_ev
    hard_hit_pct <- pa_tot$hardhit_pct
    total_bbe <- pa_tot$bbe_n

    section_header <- function(txt) {
      tags$div(
        style = paste(
          "margin:10px 0 6px 0;",
          "padding:6px 8px;",
          "border-left:4px solid rgba(128,0,0,0.9);", # maroon accent
          "background:rgba(0,0,0,0.03);",
          "border-radius:10px;",
          "font-size:12px;",
          "font-weight:800;",
          "letter-spacing:0.02em;",
          "color:#1f2933;" # darker
        ),
        txt
      )
    }

    tags$div(
      style = "display:grid; grid-template-columns:1fr; gap:10px; margin-top:10px;",

      section_header("Usage"),
      kpi_mini("Pitches", format(total_pitches, big.mark = ",")),
      kpi_mini("PA-ending", format(total_pas, big.mark = ",")),
      kpi_mini("BBE", format(total_bbe, big.mark = ",")),

      section_header("Attack"),
      kpi_mini("Zone%", scales::label_percent(accuracy = 0.1)(zone_pct)),
      kpi_mini(
        "First-pitch strike%",
        scales::label_percent(accuracy = 0.1)(fps_pct)
      ),
      kpi_mini("CSW%", scales::label_percent(accuracy = 0.1)(csw_pct)),
      kpi_mini("Whiff%", scales::label_percent(accuracy = 0.1)(whiff_pct)),
      kpi_mini("PutAway%", scales::label_percent(accuracy = 0.1)(putaway_pct)),
      kpi_mini("K%", scales::label_percent(accuracy = 0.1)(k_pct)),
      kpi_mini("BB%", scales::label_percent(accuracy = 0.1)(bb_pct)),

      section_header("Contact"),
      kpi_mini(
        "Avg EV",
        scales::label_number(accuracy = 0.1, suffix = " mph")(avg_ev)
      ),
      kpi_mini("HardHit%", scales::label_percent(accuracy = 0.1)(hard_hit_pct))
    )
  })

  output$pitchlab_movement <- renderPlotly({
    tbl <- pitchlab_data()
    validate(need(
      nrow(tbl) > 0,
      "No Statcast pitching data available for this range."
    ))
    plot_pitch_movement(tbl, ellipses = TRUE)
  })

  output$pitchlab_velocity <- renderPlotly({
    tbl <- pitchlab_data()
    validate(need(
      nrow(tbl) > 0,
      "No Statcast pitching data available for this range."
    ))
    plot_pitch_velocity(tbl)
  })
}
