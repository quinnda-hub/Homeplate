# Create cache for player head shots.
dir.create("www/players", FALSE)
temp_dir <- "www/players"

cf <- cache_disk(max_age = 24 * 60 * 60)
dh <- memoise(downloadHeadshot, cache = cf)
sc_cache <- cache_disk(max_age =  6 * 60 * 60)
sc_contact <- memoise(statcastContact, cache = sc_cache)

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

    if (identical(input$stats, "Contact Lab")) {
      return()
    }

    choices <- if (input$stats == "Pitching") {
      c("ERA", "FIP", "Game Score", "Innings Pitched")
    } else {
      c("AVG", "OBP", "SLG", "OPS")
    }

    updateSelectizeInput(session, "stat_search", choices = choices, server = FALSE)

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
  output$head_shot <- renderImage({
    req(input$player_search)

    path <- paste0("www/players",
                   "/",
                   input$player_search,
                   ".png")

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
      alt = global_vals$active_players()[player_id == input$player_search, name]
    )
  }, deleteFile = FALSE)

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
      pitchingLogsPlot(plogs[player_id == input$player_search], stat, input$rolling_window)
    } else {
      blogs <- global_vals$batting_logs()
      battingLogsPlot(blogs[player_id == input$player_search], stat, input$rolling_window)
    }

  })

  # If a player's primary position is a Pitcher, display pitching stats
  # automatically. Else, show their batting stats.
  # This also controls inputs for the plot under the head shot.
  observeEvent(input$player_search, {
    req(input$player_search)

    # If user is currently on Contact Lab, don't override their tab choice
    if (identical(isolate(input$stats), "Contact Lab")) {
      return()
    }

    position <- global_vals$active_players()[player_id == input$player_search, position]
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
    req(input$stats == "Contact Lab",
        input$player_search,
        input$contact_dates)

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
      updateSelectizeInput(session, "contact_pitch_type",
                           choices = "All",
                           selected = "All")
      updateSelectizeInput(session, "contact_bb_type",
                           choices = "All",
                           selected = "All")
      return()
    }

    updateSelectizeInput(
      session,
      "contact_pitch_type",
      choices = c("All", sort(unique(dt$pitch_type))),
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

    if (!is.null(input$contact_pitch_type) &&
        input$contact_pitch_type != "All") {
      dt <- dt[pitch_type == input$contact_pitch_type]
    }

    if (!is.null(input$contact_bb_type) &&
        input$contact_bb_type != "All") {
      dt <- dt[bb_type == input$contact_bb_type]
    }

    dt
  })

  output$contact_summary <- renderUI({
    dt <- contact_data()

    validate(
      need(nrow(dt) > 0, "No Statcast contact data available for this range.")
    )

    balls_in_play <- dt[!is.na(launch_speed) & !is.na(launch_angle)]

    validate(
      need(nrow(balls_in_play) > 0, "No batted-ball data for the selected filters.")
    )

    hard_hit_rate <- mean(balls_in_play$launch_speed >= 95, na.rm = TRUE)
    sweet_spot_rate <- mean(balls_in_play$launch_angle >= 8 &
                              balls_in_play$launch_angle <= 32, na.rm = TRUE)

    tags$div(
      tags$ul(
        tags$li(paste0("Batted balls: ", nrow(balls_in_play))),
        tags$li(paste0("Avg EV: ",
                       round(mean(balls_in_play$launch_speed, na.rm = TRUE), 1),
                       " mph")),
        tags$li(paste0("Avg LA: ",
                       round(mean(balls_in_play$launch_angle, na.rm = TRUE), 1),
                       "°")),
        tags$li(paste0("Hard-hit rate: ",
                       sprintf("%.1f%%", hard_hit_rate * 100))),
        tags$li(paste0("Sweet-spot rate: ",
                       sprintf("%.1f%%", sweet_spot_rate * 100)))
      )
    )
  })

  output$contact_ev_angle <- renderPlotly({
    dt <- contact_data()

    validate(
      need(nrow(dt) > 0, "No Statcast contact data available for this range.")
    )

    balls_in_play <- dt[!is.na(launch_speed) & !is.na(launch_angle)]

    validate(
      need(nrow(balls_in_play) > 0, "No batted-ball data for the selected filters.")
    )

    plotContactEVAngle(dt)
  })

  output$contact_spray <- renderPlotly({
    dt <- contact_data()

    validate(
      need(nrow(dt) > 0, "No Statcast contact data available for this range.")
    )

    balls_in_play <- dt[!is.na(hc_x) & !is.na(hc_y)]

    validate(
      need(nrow(balls_in_play) > 0, "No spray chart data for the selected filters.")
    )

    plotContactSprayChart(dt)
  })
}
