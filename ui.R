#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
ui <- function(request) {
  shiny::tagList(
    shinyjs::useShinyjs(),

    shiny::tags$head(
      shiny::tags$style(shiny::HTML(
        "
        /* Hide transient Shiny errors (your hack) */
        .shiny-output-error { visibility: hidden; }
        .shiny-output-error:before { visibility: hidden; }

        /* Make reactable stretch nicely inside containers */
        .reactable { height: 100% !important; }

        /* If you wrap outputs with withSpinner(), let wrappers stretch */
        .shiny-spinner-output-container { height: 100%; }
        .shiny-spinner-output-container > .load-container { height: 100%; }

        /* ---------- Player Stats right side: fill viewport ---------- */
        .player-right {
          height: calc(100vh - 56px); /* tweak if navbar height differs */
          display: flex;
          flex-direction: column;
          min-height: 0;
        }

        /* Wrapper around tabsetPanel */
        .player-tabs {
          flex: 1;
          min-height: 0;
        }

        /* tabsetPanel renders a .tabbable with .tab-content inside it */
        .player-tabs > .tabbable {
          height: 100%;
          display: flex;
          flex-direction: column;
          min-height: 0;
        }

        .player-tabs .tab-content {
          flex: 1;
          min-height: 0;
        }

        .player-tabs .tab-pane {
          height: 100%;
          min-height: 0;
        }

        /* ---------- 50/50 split inside each tab ---------- */
        .split-rows {
          height: 100%;
          display: grid;
          grid-template-rows: 1fr 1fr;
          gap: 10px;
          min-height: 0;
        }
        .split-rows > * { min-height: 0; }

        /* Card-ish look without requiring bslib card features */
        .panel-card {
          border: 1px solid rgba(0,0,0,0.1);
          border-radius: 0.5rem;
          padding: 0.5rem;
          background: white;
          min-height: 0;
        }

        /* make sidebar card stand out */
        .panel-card.sidebar-card {
          background: rgba(250,250,250,1);
          border: 1px solid rgba(0,0,0,0.14);
          box-shadow: 0 2px 8px rgba(0,0,0,0.06);
        }

        /* ---------- Contact Lab layout ---------- */
        .contact-layout {
          height: 100%;
          display: grid;
          grid-template-columns: 320px 1fr; /* tweak sidebar width */
          gap: 10px;
          min-height: 0;
        }

        .contact-layout > * { min-height: 0; }

        .contact-plots {
          height: 100%;
          display: grid;
          grid-template-rows: 1fr 1fr;
          gap: 10px;
          min-height: 0;
        }

        .contact-plots > * { min-height: 0; }

         /* ---------- Pitching Lab layout ---------- */
        .pitching-layout {
          height: 100%;
          display: grid;
          grid-template-columns: 320px 1fr;
          gap: 10px;
          min-height: 0;
        }

        .pitching-layout > * { min-height: 0; }

        .pitching-plots {
          height: 100%;
          display: grid;
          grid-template-rows: 1fr 1fr;
          gap: 10px;
          min-height: 0;
        }

        .pitching-plots > * { min-height: 0; }

        /* ---------- Sidebar card: inputs + scrollable summary ---------- */
        .sidebar-card {
          display: flex;
          flex-direction: column;
          height: 100%;
          min-height: 0;          /* crucial */
        }

        /* Summary area scrolls instead of overflowing the card */
        .sidebar-summary {
          flex: 1;
          min-height: 0;          /* crucial */
          overflow-y: auto;
          padding-top: 10px;
        }

        /* Optional: tighten spacing of inputs inside sidebar */
        .sidebar-card .form-group { margin-bottom: 8px; }
        .sidebar-card .selectize-control { margin-bottom: 8px; }
      "
      ))
    ),

    shiny::navbarPage(
      title = "Homeplate",
      theme = bslib::bs_theme(
        bg = "white",
        fg = "black",
        primary = "Maroon",
        base_font = bslib::font_google("Montserrat"),
        version = 5
      ),
      inverse = TRUE,
      id = "home",

      # ---------------- Standings ----------------
      shiny::tabPanel(
        "Standings",
        shiny::fluidPage(
          bslib::layout_column_wrap(
            width = 1 / 2,
            reactable::reactableOutput("al_east"),
            reactable::reactableOutput("nl_east"),
            reactable::reactableOutput("al_central"),
            reactable::reactableOutput("nl_central"),
            reactable::reactableOutput("al_west"),
            reactable::reactableOutput("nl_west")
          )
        )
      ),

      # ---------------- Player Stats ----------------
      shiny::tabPanel(
        "Player Stats",
        shiny::fluidPage(
          shiny::fluidRow(
            shiny::column(
              2,
              align = "center",
              shiny::div(
                id = "headshot",
                shiny::imageOutput("head_shot", inline = TRUE),
                shiny::div(
                  id = "_summary",
                  shiny::h6(shiny::uiOutput("summary")),
                  shiny::div(
                    id = "_plot",
                    style = "margin-left: -35px",
                    plotly::plotlyOutput(
                      "plot",
                      height = "180px",
                      width = "275px"
                    )
                  )
                ),
                shiny::div(
                  id = "controls",
                  shiny::selectizeInput(
                    "stat_search",
                    choices = NULL,
                    multiple = FALSE,
                    label = NULL
                  ),
                  shinyWidgets::chooseSliderSkin("Flat", color = "Maroon"),
                  shiny::sliderInput(
                    "rolling_window",
                    "Smoothness",
                    min = 1,
                    max = 30,
                    value = 6
                  )
                )
              )
            ),

            shiny::column(
              10,
              align = "center",

              shiny::div(
                class = "player-right",

                shiny::div(
                  id = "header",
                  labeledInput(
                    "player",
                    "Search for a player:",
                    shiny::selectizeInput(
                      "player_search",
                      choices = NULL,
                      multiple = FALSE,
                      label = NULL
                    )
                  )
                ),

                shiny::div(
                  class = "player-tabs",
                  shiny::tabsetPanel(
                    id = "stats",

                    shiny::tabPanel(
                      "Batting",
                      shiny::div(
                        class = "split-rows",
                        shiny::div(
                          class = "panel-card",
                          reactable::reactableOutput(
                            "batting_logs",
                            height = "100%",
                            width = "100%"
                          ) |>
                            shinycssloaders::withSpinner(color = "#0DC5C1")
                        ),
                        shiny::div(
                          class = "panel-card",
                          reactable::reactableOutput(
                            "batting_stats",
                            height = "100%",
                            width = "100%"
                          ) |>
                            shinycssloaders::withSpinner(color = "white")
                        )
                      )
                    ),

                    shiny::tabPanel(
                      "Pitching",
                      shiny::div(
                        class = "split-rows",
                        shiny::div(
                          class = "panel-card",
                          reactable::reactableOutput(
                            "pitching_logs",
                            height = "100%",
                            width = "100%"
                          ) |>
                            shinycssloaders::withSpinner(color = "#0DC5C1")
                        ),
                        shiny::div(
                          class = "panel-card",
                          reactable::reactableOutput(
                            "pitching_stats",
                            height = "100%",
                            width = "100%"
                          ) |>
                            shinycssloaders::withSpinner(color = "white")
                        ),
                      )
                    ),
                    shiny::tabPanel(
                      "Contact Lab",
                      shiny::div(
                        class = "contact-layout",

                        # Sidebar
                        shiny::div(
                          class = "panel-card sidebar-card",
                          shiny::dateRangeInput(
                            "contact_dates",
                            "Date range",
                            start = Sys.Date() - 30,
                            end = Sys.Date()
                          ),
                          shiny::selectizeInput(
                            "contact_pitch_type",
                            "Pitch type",
                            choices = "All",
                            multiple = FALSE
                          ),
                          shiny::selectizeInput(
                            "contact_bb_type",
                            "Batted-ball type",
                            choices = "All",
                            multiple = FALSE
                          ),
                          shiny::div(
                            class = "sidebar-summary",
                            shiny::uiOutput("contact_summary")
                          )
                        ),

                        # Plots (two card containers)
                        shiny::div(
                          class = "contact-plots",

                          shiny::div(
                            class = "panel-card",
                            plotly::plotlyOutput(
                              "contact_ev_density",
                              height = "100%",
                              width = "100%"
                            ) |>
                              shinycssloaders::withSpinner(color = "#0DC5C1")
                          ),

                          shiny::div(
                            class = "panel-card",
                            plotly::plotlyOutput(
                              "contact_spray",
                              height = "100%",
                              width = "100%"
                            ) |>
                              shinycssloaders::withSpinner(color = "white")
                          )
                        )
                      )
                    ),
                    shiny::tabPanel(
                      "Pitching Lab",
                      shiny::div(
                        class = "pitching-layout",

                        shiny::div(
                          class = "panel-card sidebar-card",
                          shiny::dateRangeInput(
                            "pitchlab_dates",
                            "Date range",
                            start = Sys.Date() - 30,
                            end = Sys.Date()
                          ),
                          shiny::selectizeInput(
                            "pitchlab_pitch_type",
                            "Pitch type",
                            choices = "All",
                            multiple = FALSE
                          ),
                          shiny::selectizeInput(
                            "pitchlab_count_state",
                            "Count state",
                            choices = c("All", "Ahead", "Even", "Behind"),
                            selected = "All",
                            multiple = FALSE
                          ),
                          shiny::selectizeInput(
                            "pitchlab_tto",
                            "Times through order",
                            choices = c("All", "1", "2", "3+"),
                            selected = "All",
                            multiple = FALSE
                          ),
                          shiny::selectizeInput(
                            "pitchlab_batter_hand",
                            "Batter handedness",
                            choices = c("All", "L", "R"),
                            selected = "All",
                            multiple = FALSE
                          ),
                          shiny::selectizeInput(
                            "pitchlab_zone_filter",
                            "Zone filter",
                            choices = c("All", "In Zone", "Out of Zone"),
                            selected = "All",
                            multiple = FALSE
                          ),
                          shiny::selectizeInput(
                            "pitchlab_runners",
                            "Base state",
                            choices = c("All", "Bases Empty", "Runners On"),
                            selected = "All",
                            multiple = FALSE
                          ),
                          shiny::checkboxInput(
                            "pitchlab_two_strike",
                            "Two-strike pitches only",
                            value = FALSE
                          ),
                          shiny::div(
                            class = "sidebar-summary",
                            shiny::uiOutput("pitchlab_summary")
                          )
                        ),

                        shiny::div(
                          class = "pitching-plots",

                          shiny::div(
                            class = "panel-card",
                            plotly::plotlyOutput(
                              "pitchlab_movement",
                              height = "100%",
                              width = "100%"
                            ) |>
                              shinycssloaders::withSpinner(color = "#0DC5C1")
                          ),

                          shiny::div(
                            class = "panel-card",
                            plotly::plotlyOutput(
                              "pitchlab_velocity",
                              height = "100%",
                              width = "100%"
                            ) |>
                              shinycssloaders::withSpinner(color = "white")
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}
