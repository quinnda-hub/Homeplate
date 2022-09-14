# Create cache for player head shots.
dir.create("www/players", FALSE)
temp_dir <- "www/players"

cf <- cache_disk(max_age = 24 * 60 * 60)
dh <- memoise(downloadHeadshot, cache = cf)

# Contains all the values that should be shared across sessions.
global_vals <-
  reactiveValues(
    standings = getStandings(format(Sys.Date(), "%Y")),
    active_players = reactivePoll(
      6e4,
      session = NULL,
      checkFunc = function() {
        Sys.Date()
      },
      valueFunc = function() {
        teamIds() |>
          getRoster()
      }
    ),
    player_info = reactivePoll(
      6e4,
      session = NULL,
      checkFunc = function() {
        Sys.Date()
      },
      valueFunc = function() {
        global_vals$active_players()$player_id |> playerInfo()
      }
    ),
    batting_logs = reactivePoll(
      6e4,
      session = NULL,
      checkFunc = function() {
        Sys.Date()
      },
      valueFunc = function() {
        global_vals$active_players()$player_id |>
          gameLogs(format(Sys.Date(), "%Y"))
      }
    ),
    batting_stats = reactivePoll(
      6e4,
      session = NULL,
      checkFunc = function() {
        Sys.Date()
      },
      valueFunc = function() {
        global_vals$active_players()$player_id |>
          yearStats()
      }
    ),
    pitching_logs = reactivePoll(
      6e4,
      session = NULL,
      checkFunc = function() {
        Sys.Date()
      },
      valueFunc = function() {
        global_vals$active_players()$player_id |>
          gameLogs(format(Sys.Date(), "%Y"), "pitching")
      }
    ),
    pitching_stats = reactivePoll(
      6e4,
      session = NULL,
      checkFunc = function() {
        Sys.Date()
      },
      valueFunc = function() {
        global_vals$active_players()$player_id |>
          yearStats("pitching")
      }
    )
  )

server <- function(input, output, session) {
  # Update the standings.
  observe({
    invalidateLater(3e5, session = NULL)
    global_vals$standings <- getStandings(format(Sys.Date(), "%Y"))
  })

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

    pos <- global_vals$player_info()[id == input$player_search, primary_position]

    choices <- if (pos == "Pitcher") {
      c("ERA", "FIP", "Game Score", "Innings Pitched")
    } else {
      c("AVG", "OBP", "SLG", "OPS")
    }

    updateSelectizeInput(
      inputId = "stat_search",
      server = FALSE,
      choices = choices
    )
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
      contentType = "image/jpeg",
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

    pos <- global_vals$player_info()[id == input$player_search, primary_position]

    stat <- if (input$stat_search == "Innings Pitched") {
      "innings_pitched"
    } else if (input$stat_search == "Game Score") {
      "game_score"
    } else {
      tolower(input$stat_search)
    }

    if (pos == "Pitcher") {
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

    position <- global_vals$active_players()[player_id == input$player_search, position]

    selection <- if (position == "Pitcher") "Pitching" else "Batting"
    val <- if (position == "Pitcher") 3 else 6
    max <- if (position == "Pitcher") 14 else 30

    updateTabsetPanel(session, "stats",
                      selected = selection)

    updateSliderInput(session, "rolling_window", value = val, max = max)

  })
}
