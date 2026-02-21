statcastContact <- function(player_id, start_date, end_date) {
  baseballr::statcast_search(start_date = start_date,
                             end_date = end_date,
                             playerid = player_id) |>
    as.data.table()
}

make_field_shapes <- function(xr, yr,
                                     arc_frac = 0.96,
                                     infield_frac = 0.23,
                                     dirt_opacity = 0.28,
                                     line_col = "rgba(60,60,60,0.55)") {
  hx <- mean(xr)
  hy <- min(yr)

  r  <- min(diff(xr) / 2, diff(yr)) * arc_frac
  b  <- r * infield_frac

  # Foul lines endpoints (45 degrees visually).
  left_end  <- c(hx - r, hy + r)
  right_end <- c(hx + r, hy + r)

  # Outfield arc path.
  t <- seq(pi * 0.80, pi * 0.20, length.out = 90)
  arc_x <- hx + r * cos(t)
  arc_y <- hy + r * sin(t)
  arc_path <- paste0(
    "M ", sprintf("%.3f", arc_x[1]), ",", sprintf("%.3f", arc_y[1]),
    paste(sprintf(" L %.3f,%.3f", arc_x[-1], arc_y[-1]), collapse = "")
  )

  # Infield dirt: a smaller arc wedge + diamond outline.
  r_dirt <- b * 2.15
  td <- seq(pi * 0.78, pi * 0.22, length.out = 70)
  dirt_x <- hx + r_dirt * cos(td)
  dirt_y <- hy + r_dirt * sin(td)

  dirt_path <- paste0(
    "M ", hx, ",", hy,
    paste(sprintf(" L %.3f,%.3f", dirt_x, dirt_y), collapse = ""),
    " Z"
  )

  # Base coordinates.
  home <- c(hx, hy)
  b1   <- c(hx + b, hy + b)
  b2   <- c(hx,     hy + 2*b)
  b3   <- c(hx - b, hy + b)

  base_sq <- function(p, s = b * 0.18) {
    list(type = "rect",
         x0 = p[1] - s, x1 = p[1] + s,
         y0 = p[2] - s, y1 = p[2] + s,
         line = list(width = 1, color = "rgba(40,40,40,0.7)"),
         fillcolor = "rgba(255,255,255,0.9)")
  }

  list(
    list(type = "path", path = dirt_path,
         line = list(width = 0, color = "rgba(0,0,0,0)"),
         fillcolor = sprintf("rgba(210,180,140,%.2f)", dirt_opacity),
         layer = "below"),

    # Basepaths / diamond outline.
    list(type = "line", x0 = home[1], y0 = home[2], x1 = b1[1], y1 = b1[2],
         line = list(width = 2, color = line_col), layer = "below"),
    list(type = "line", x0 = home[1], y0 = home[2], x1 = b3[1], y1 = b3[2],
         line = list(width = 2, color = line_col), layer = "below"),
    list(type = "line", x0 = b1[1], y0 = b1[2], x1 = b2[1], y1 = b2[2],
         line = list(width = 2, color = line_col), layer = "below"),
    list(type = "line", x0 = b3[1], y0 = b3[2], x1 = b2[1], y1 = b2[2],
         line = list(width = 2, color = line_col), layer = "below"),

    # Foul lines.
    list(type = "line", x0 = hx, y0 = hy, x1 = left_end[1],  y1 = left_end[2],
         line = list(width = 2, color = line_col), layer = "below"),
    list(type = "line", x0 = hx, y0 = hy, x1 = right_end[1], y1 = right_end[2],
         line = list(width = 2, color = line_col), layer = "below"),

    # Outfield arc.
    list(type = "path", path = arc_path,
         line = list(width = 2, color = line_col),
         fillcolor = "rgba(0,0,0,0)",
         layer = "below"),

    # Bases.
    base_sq(b1), base_sq(b2), base_sq(b3)
  )
}

plotContactEVAngle <- function(dt) {
  balls_in_play <- dt[!is.na(launch_speed) & !is.na(launch_angle)]

  balls_in_play[, event_label := data.table::fifelse(is.na(events) | events == "", "In Play", events)]

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
    config(displayModeBar = FALSE)
}

plotContactSprayChart <- function(dt) {
  balls_in_play <- dt[!is.na(hc_x) & !is.na(hc_y)]
  balls_in_play[, hc_y := hc_y * -1]

  balls_in_play[, event_label := data.table::fifelse(is.na(events) | events == "", "In Play", events)]
  balls_in_play[, event_label := stringr::str_replace_all(event_label, "_", " ")]
  balls_in_play[, event_label := stringr::str_to_title(event_label)]

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
  balls_in_play[, event_label := factor(event_label, levels = lvl)]

  xr <- range(balls_in_play$hc_x, na.rm = TRUE)
  yr <- range(balls_in_play$hc_y, na.rm = TRUE)
  xpad <- diff(xr) * 0.05
  ypad <- diff(yr) * 0.05
  xr2 <- c(xr[1] - xpad, xr[2] + xpad)
  yr2 <- c(yr[1] - ypad, yr[2] + ypad)

  field_shapes <- make_field_shapes_pretty(xr2, yr2)

  plot_ly(
    balls_in_play,
    x = ~hc_x,
    y = ~hc_y,
    type = "scatter",
    mode = "markers",
    color = ~event_label,
    colors = pal,
    marker = list(
      size = 12,
      opacity = 0.70,
      line = list(width = 0.6, color = "rgba(0,0,0,0.25)")
    ),
    hovertemplate = paste(
      "Spray: (%{x:.0f}, %{y:.0f})",
      "<br>Result: %{fullData.name}",
      "<extra></extra>"
    )
  ) |>
    layout(
      title = "Spray Chart",
      shapes = field_shapes,
      plot_bgcolor  = "rgba(226,242,226,1)",
      paper_bgcolor = "rgba(226,242,226,1)",
      margin = list(l = 10, r = 10, t = 50, b = 60),
      legend = list(
        orientation = "h",
        x = 0.5, xanchor = "center",
        y = -0.08,
        bgcolor = "rgba(255,255,255,0.65)"
      ),
      font = list(color = "rgba(40,40,40,1)"),
      xaxis = list(title = "", range = xr2, fixedrange = TRUE,
                   showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,
                   ticks = "", showline = FALSE),
      yaxis = list(title = "", range = yr2, fixedrange = TRUE,
                   showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,
                   ticks = "", showline = FALSE,
                   scaleanchor = "x", scaleratio = 1),
      hovermode = "closest"
    ) |> config(displayModeBar = FALSE)
}
