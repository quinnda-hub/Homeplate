#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
ui <- function(request) {
  tagList(
    useShinyjs(),
    # Ugly hack for now. Will remove when I fix the Plot flashing an error
    # looking for the wrong stat when switching between hitters and pitchers.
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    navbarPage(
      title = "Homeplate",
      theme = bslib::bs_theme(
        bg = "white",
        fg = "black",
        primary = "Maroon",
        base_font = font_google("Montserrat"),
        version = 4
      ),
      inverse = TRUE,
      id = "home",
      tabPanel("Standings",
               fluidPage(
                 fluidRow(column(6,
                                 reactableOutput("al_east")),
                          column(6,
                                 reactableOutput("nl_east"))),
                 fluidRow(column(6,
                                 reactableOutput("al_central")),
                          column(6,
                                 reactableOutput("nl_central"))),
                 fluidRow(column(6,
                                 reactableOutput("al_west")),
                          column(6,
                                 reactableOutput("nl_west")))
               )),
      tabPanel("Player Stats",
               fluidPage(fluidRow(
                 column(2,
                        align = "center",
                        div(
                          id = "headshot",
                          imageOutput("head_shot",
                                      inline = TRUE),
                          div(
                            id = "_summary",
                            h6(uiOutput("summary")),
                            div(
                              id = "_plot",
                              style = "margin-left: -35px",
                              plotlyOutput("plot",
                                           height = "180px",
                                           width = "275px")
                            )
                          ),
                          div(
                            id = "controls",
                            selectizeInput(
                              "stat_search",
                              choices = NULL,
                              multiple = FALSE,
                              label = NULL
                            ),
                            chooseSliderSkin("Flat", color = "Maroon"),
                            sliderInput(
                              "rolling_window",
                              "Smoothness",
                              min = 1,
                              max = 30,
                              value = 6
                            )
                          )

                        )),
                 column(
                   10,
                   align = "center",
                   div(
                     id = "header",
                     labeledInput(
                       "player",
                       "Search for a player:",
                       selectizeInput(
                         "player_search",
                         choices = NULL,
                         multiple = FALSE,
                         label = NULL
                       )
                     )
                   ),
                   tabsetPanel(
                     id = "stats",
                     tabPanel(
                       "Batting",
                       reactableOutput("batting_logs",
                                       height = "auto",
                                       width = "100%") |> withSpinner(color = "#0DC5C1"),
                       br(),
                       reactableOutput("batting_stats",
                                       height = "auto",
                                       width = "100%") |> withSpinner(color = "white")
                     ),
                     tabPanel(
                       "Pitching",
                       reactableOutput("pitching_logs",
                                       height = "auto",
                                       width = "100%") |> withSpinner(color = "#0DC5C1"),
                       br(),
                       reactableOutput("pitching_stats",
                                       height = "auto",
                                       width = "100%")
                     )
                   )
                 )
               )))
    )
  )}
