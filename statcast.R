plotContactEVAngle <- function(dt) {
  balls_in_play <- as.data.table(dt)[!is.na(launch_speed) & !is.na(launch_angle)]

  balls_in_play[, event_label := fifelse(events == "", "In Play", events)]
  balls_in_play[, event_label := str_replace_all(event_label, "_", " ")]
  balls_in_play[, event_label := str_to_title(event_label)]

  pal <- c(
    "Field Out"                  = "#B0B0B0",
    "Force Out"                  = "#8F8F8F",
    "Grounded Into Double Play"  = "#6F6F6F",
    "Fielders Choice"            = "#A7A7A7",
    "Field Error"                = "#8BB8FF",
    "In Play"                    = "#5BC0EB",
    "Single"                     = "#74C476",
    "Double"                     = "#31A354",
    "Triple"                     = "#FD8D3C",
    "Home Run"                   = "#DE2D26"
  )

  lvl <- c(
    "Field Out", "Force Out", "Grounded Into Double Play", "Fielders Choice", "Field Error",
    "In Play", "Single", "Double", "Triple", "Home Run"
  )
  pal_vec <- unname(pal[lvl])

  balls_in_play[, event_label := factor(event_label, levels = lvl)]

  xr <- range(balls_in_play$launch_angle, na.rm = TRUE)
  yr <- range(balls_in_play$launch_speed, na.rm = TRUE)

  xpad <- diff(xr) * 0.03
  ypad <- diff(yr) * 0.03

  plot_ly(
    balls_in_play,
    x = ~launch_angle,
    y = ~launch_speed,
    type = "scatter",
    mode = "markers",
    color = ~event_label,
    colors = pal,
    marker = list(size = 11, opacity = 0.65),
    hovertemplate = paste(
      "EV: %{y:.1f} mph",
      "<br>LA: %{x:.1f}°",
      "<br>Result: %{fullData.name}",
      "<extra></extra>"
    )
  ) |>
    layout(
      title = "Exit Velocity vs Launch Angle",
      xaxis = list(title = "Launch Angle (°)", range = c(xr[1] - xpad, xr[2] + xpad)),
      yaxis = list(title = "Exit Velocity (mph)", range = c(yr[1] - ypad, yr[2] + ypad)),
      legend = list(orientation = "h", y = -0.2),
      hovermode = "closest"
    ) |>
    config(modeBarButtonsToRemove = c("zoom", "pan", "scale", "zoomIn", "zoomOut",
                                      "autoScale", "resetScale", "hoverClosestCartesian",
                                      "hoverCompareCartesian"),
           toImageButtonOptions = list(format = "png",
                                       height = 1080,
                                       width = 1920,
                                       scale = 3,
                                       filename = head(dt$name, 1)),
           displaylogo = FALSE)
}


