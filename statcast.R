statcast_contact <- function(player_id, start_date, end_date) {
  baseballr::statcast_search(
    start_date = start_date,
    end_date = end_date,
    playerid = player_id
  ) |>
    as.data.table()
}

statcast_pitch <- function(player_id, start_date, end_date) {
  baseballr::statcast_search(
    start_date = start_date,
    end_date = end_date,
    playerid = player_id,
    player_type = "pitcher"
  )
}

### Pitching.
pitchTypePalette <- function(pitch_types = NULL) {
  base_pal <- c(
    "4-Seam Fastball" = "#E41A1C",
    "Sinker" = "#FF7F00",
    "Cutter" = "#A65628",
    "Splitter" = "#F781BF",
    "Slider" = "#377EB8",
    "Sweeper" = "#4DAF4A",
    "Curveball" = "#984EA3",
    "Knuckle Curve" = "#6A3D9A",
    "Slurve" = "#1B9E77",
    "Changeup" = "#FFD92F",
    "Slow Curver" = "#66A61E",
    "Knuckleball" = "#999999",
    "Eephus" = "#17BECF",
    "Forkball" = "#8DD3C7",
    "Screwball" = "#BC80BD"
  )

  if (is.null(pitch_types)) {
    return(base_pal)
  }

  pitch_types <- unique(stats::na.omit(as.character(pitch_types)))
  unknown <- setdiff(pitch_types, names(base_pal))

  if (length(unknown)) {
    extra <- grDevices::hcl.colors(length(unknown), "Dark 3")
    names(extra) <- unknown
    base_pal <- c(base_pal, extra)
  }

  base_pal[pitch_types]
}

summarise_pitch_level <- function(tbl) {
  swing_desc <- c(
    "swinging_strike",
    "swinging_strike_blocked",
    "foul",
    "foul_tip",
    "hit_into_play",
    "hit_into_play_no_out",
    "hit_into_play_score"
  )

  whiff_desc <- c("swinging_strike", "swinging_strike_blocked", "foul_tip")

  d <- tbl |>
    tibble::as_tibble() |>
    dplyr::filter(!is.na(pitch_name), pitch_name != "") |>
    dplyr::mutate(
      is_swing = !is.na(description) & description %in% swing_desc,
      is_whiff = !is.na(description) & description %in% whiff_desc,
      two_strike = !is.na(strikes) & strikes == 2,
      is_putaway = two_strike &
        !is.na(events) &
        events %in% c("strikeout", "strikeout_double_play")
    )

  total_pitches <- nrow(d)

  d |>
    dplyr::group_by(pitch_name) |>
    dplyr::summarise(
      pitch_n = dplyr::n(),
      pitch_pct = pitch_n / total_pitches,
      mph = mean(release_speed, na.rm = TRUE),
      spin = if ("release_spin_rate" %in% names(d)) {
        mean(release_spin_rate, na.rm = TRUE)
      } else {
        NA_real_
      },
      ext = if ("release_extension" %in% names(d)) {
        mean(release_extension, na.rm = TRUE)
      } else {
        NA_real_
      },

      whiff_pct = {
        swings_n <- sum(is_swing, na.rm = TRUE)
        if (swings_n == 0) NA_real_ else sum(is_whiff, na.rm = TRUE) / swings_n
      },

      putaway_pct = {
        two_strike_n <- sum(two_strike, na.rm = TRUE)
        if (two_strike_n == 0) {
          NA_real_
        } else {
          sum(is_putaway, na.rm = TRUE) / two_strike_n
        }
      },

      .groups = "drop"
    )
}

summarise_pitch_attack <- function(tbl) {
  whiff_desc <- c("swinging_strike", "swinging_strike_blocked", "foul_tip")

  strike_desc <- c(
    "called_strike",
    "swinging_strike",
    "swinging_strike_blocked",
    "foul",
    "foul_tip",
    "foul_bunt",
    "hit_into_play",
    "hit_into_play_no_out",
    "hit_into_play_score"
  )

  d <- tbl |>
    tibble::as_tibble() |>
    dplyr::filter(!is.na(pitch_name), pitch_name != "") |>
    dplyr::mutate(
      is_whiff = !is.na(description) & description %in% whiff_desc,
      is_called_strike = !is.na(description) & description == "called_strike",
      in_zone = !is.na(zone) & zone %in% 1:9,
      is_fps_pitch = !is.na(balls) &
        !is.na(strikes) &
        balls == 0 &
        strikes == 0,
      is_fps_strike = (is_fps_pitch %in% TRUE) &
        ((!is.na(type) & type == "S") |
          (!is.na(description) & description %in% strike_desc))
    )

  zone_pct <- if (nrow(d) == 0) NA_real_ else mean(d$in_zone, na.rm = TRUE)
  csw_pct <- if (nrow(d) == 0) {
    NA_real_
  } else {
    mean((d$is_called_strike | d$is_whiff), na.rm = TRUE)
  }

  fps_pct <- {
    fps_n <- sum(d$is_fps_pitch %in% TRUE, na.rm = TRUE)
    if (fps_n == 0) {
      NA_real_
    } else {
      sum(d$is_fps_strike %in% TRUE, na.rm = TRUE) / fps_n
    }
  }

  tibble::tibble(
    pitches = nrow(d),
    zone_pct = zone_pct,
    csw_pct = csw_pct,
    fps_pct = fps_pct
  )
}

summarise_pa_last <- function(tbl) {
  tbl |>
    tibble::as_tibble() |>
    dplyr::filter(
      !is.na(game_pk),
      !is.na(at_bat_number),
      !is.na(pitch_number),
      !is.na(pitch_name),
      pitch_name != ""
    ) |>
    dplyr::group_by(game_pk, at_bat_number) |>
    dplyr::slice_max(pitch_number, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::filter(
      (!is.na(type) & type == "X") |
        (!is.na(events) & events != "")
    )
}

summarise_pa_ending <- function(tbl) {
  d_last <- summarise_pa_last(tbl)

  is_so <- !is.na(d_last$events) &
    d_last$events %in% c("strikeout", "strikeout_double_play")
  is_bb <- !is.na(d_last$events) & d_last$events == "walk"
  is_ibb <- !is.na(d_last$events) & d_last$events == "intent_walk"
  is_hbp <- !is.na(d_last$events) & d_last$events == "hit_by_pitch"
  is_sf <- !is.na(d_last$events) & d_last$events == "sac_fly"
  is_sh <- !is.na(d_last$events) &
    d_last$events %in% c("sac_bunt", "sac_bunt_double_play")
  is_ci <- !is.na(d_last$events) & d_last$events == "catcher_interf"

  is_hit <- !is.na(d_last$events) &
    d_last$events %in% c("single", "double", "triple", "home_run")
  n1 <- !is.na(d_last$events) & d_last$events == "single"
  n2 <- !is.na(d_last$events) & d_last$events == "double"
  n3 <- !is.na(d_last$events) & d_last$events == "triple"
  n4 <- !is.na(d_last$events) & d_last$events == "home_run"

  is_ab <- !(is_bb | is_ibb | is_hbp | is_sf | is_sh | is_ci)

  is_bbe <- !is.na(d_last$type) & d_last$type == "X"

  has_woba_denom <- "woba_denom" %in% names(d_last)
  has_woba_val <- "woba_value" %in% names(d_last)
  has_xwoba <- "estimated_woba_using_speedangle" %in% names(d_last)
  has_xba <- "estimated_ba_using_speedangle" %in% names(d_last)
  has_xslg <- "estimated_slg_using_speedangle" %in% names(d_last)
  has_ev <- "launch_speed" %in% names(d_last)
  has_la <- "launch_angle" %in% names(d_last)

  d_last |>
    dplyr::mutate(
      is_ab = is_ab,
      is_hit = is_hit,
      n1 = n1,
      n2 = n2,
      n3 = n3,
      n4 = n4,
      is_so = is_so,
      is_bbe = is_bbe
    ) |>
    dplyr::group_by(pitch_name) |>
    dplyr::summarise(
      pa = dplyr::n(),
      ab = sum(is_ab, na.rm = TRUE),
      h = sum(is_hit, na.rm = TRUE),
      `1b` = sum(n1, na.rm = TRUE),
      `2b` = sum(n2, na.rm = TRUE),
      `3b` = sum(n3, na.rm = TRUE),
      hr = sum(n4, na.rm = TRUE),
      so = sum(is_so, na.rm = TRUE),
      bbe = sum(is_bbe, na.rm = TRUE),

      ba = if (ab == 0) NA_real_ else h / ab,
      slg = if (ab == 0) {
        NA_real_
      } else {
        (`1b` + 2 * `2b` + 3 * `3b` + 4 * hr) / ab
      },

      woba = {
        if (!(has_woba_val && has_woba_denom)) {
          NA_real_
        } else {
          denom <- sum(dplyr::coalesce(woba_denom, 0L), na.rm = TRUE)
          if (denom == 0) {
            NA_real_
          } else {
            sum(dplyr::coalesce(woba_value, 0), na.rm = TRUE) / denom
          }
        }
      },

      xwoba = {
        if (!(has_woba_denom && (has_xwoba || has_woba_val))) {
          NA_real_
        } else {
          denom <- sum(dplyr::coalesce(woba_denom, 0L), na.rm = TRUE)
          if (denom == 0) {
            NA_real_
          } else {
            num <- dplyr::coalesce(
              if (has_xwoba) estimated_woba_using_speedangle else NA_real_,
              if (has_woba_val) woba_value else NA_real_,
              0
            )
            sum(num, na.rm = TRUE) / denom
          }
        }
      },

      xba = {
        if (!has_xba || ab == 0) {
          NA_real_
        } else {
          sum(
            dplyr::coalesce(estimated_ba_using_speedangle, 0) *
              (is_ab %in% TRUE),
            na.rm = TRUE
          ) /
            ab
        }
      },
      xslg = {
        if (!has_xslg || ab == 0) {
          NA_real_
        } else {
          sum(
            dplyr::coalesce(estimated_slg_using_speedangle, 0) *
              (is_ab %in% TRUE),
            na.rm = TRUE
          ) /
            ab
        }
      },

      ev = {
        if (!has_ev) {
          NA_real_
        } else {
          idx <- which(is_bbe %in% TRUE)
          if (length(idx) == 0) {
            NA_real_
          } else {
            mean(launch_speed[idx], na.rm = TRUE)
          }
        }
      },
      la = {
        if (!has_la) {
          NA_real_
        } else {
          idx <- which(is_bbe %in% TRUE)
          if (length(idx) == 0) {
            NA_real_
          } else {
            mean(launch_angle[idx], na.rm = TRUE)
          }
        }
      },
      hardhit = {
        if (!has_ev) {
          NA_real_
        } else {
          idx <- which((is_bbe %in% TRUE) & !is.na(launch_speed))
          if (length(idx) == 0) {
            NA_real_
          } else {
            mean(launch_speed[idx] >= 95, na.rm = TRUE)
          }
        }
      },

      .groups = "drop"
    )
}

pitch_movement_data <- function(tbl) {
  stopifnot(is.data.frame(tbl))

  hb_col <- dplyr::case_when(
    "horizontal_break" %in% names(tbl) ~ "horizontal_break",
    "pfx_x" %in% names(tbl) ~ "pfx_x",
    "hb" %in% names(tbl) ~ "hb",
    TRUE ~ NA_character_
  )

  ivb_col <- dplyr::case_when(
    "induced_vertical_break" %in% names(tbl) ~ "induced_vertical_break",
    "ivb" %in% names(tbl) ~ "ivb",
    "pfx_z" %in% names(tbl) ~ "pfx_z",
    TRUE ~ NA_character_
  )

  out <- tbl |>
    tibble::as_tibble() |>
    dplyr::mutate(
      horizontal_break = if (is.na(hb_col)) {
        NA_real_
      } else {
        as.numeric(.data[[hb_col]])
      },
      induced_vertical_break = if (is.na(ivb_col)) {
        NA_real_
      } else {
        as.numeric(.data[[ivb_col]])
      },
      horizontal_break = if (!is.na(hb_col) && hb_col == "pfx_x") {
        horizontal_break * 12
      } else {
        horizontal_break
      },
      induced_vertical_break = if (!is.na(ivb_col) && ivb_col == "pfx_z") {
        induced_vertical_break * 12
      } else {
        induced_vertical_break
      }
    )

  out
}

plot_pitch_movement <- function(tbl, savant_view = TRUE, ellipses = FALSE) {
  movement <- tbl |>
    tibble::as_tibble() |>
    pitch_movement_data() |>
    dplyr::filter(
      !is.na(pitch_name),
      !pitch_name %in% c("", "Pitch Out"),
      !is.na(horizontal_break),
      !is.na(induced_vertical_break)
    )

  # Savant convention: catcher/batter view with 1B on the left, 3B on the right.
  # This means we flip HB relative to the Statcast HB sign you're currently plotting.
  if (savant_view) {
    movement <- movement |>
      dplyr::mutate(horizontal_break = -horizontal_break)
  }

  movement <- movement |>
    dplyr::mutate(hv_velo = sprintf("%.1f", release_speed))

  pal <- pitchTypePalette(movement$pitch_name)

  centers <- movement |>
    dplyr::group_by(pitch_name) |>
    dplyr::summarise(
      hb = stats::median(horizontal_break, na.rm = TRUE),
      ivb = stats::median(induced_vertical_break, na.rm = TRUE),
      n = dplyr::n(),
      velo = mean(release_speed, na.rm = TRUE),
      .groups = "drop"
    )

  p <- plotly::plot_ly(
    data = movement,
    x = ~horizontal_break,
    y = ~induced_vertical_break,
    type = "scatter",
    mode = "markers",
    color = ~pitch_name,
    colors = pal,
    text = ~hv_velo,
    marker = list(opacity = 0.65),
    hovertemplate = paste(
      "Pitch: %{fullData.name}",
      "<br>Velo: %{text} mph",
      "<br>HB: %{x:.1f} in",
      "<br>IVB: %{y:.1f} in",
      "<extra></extra>"
    )
  )

  if (ellipses) {
    ellipse_pts <- function(hb, ivb, level = 0.68, n = 80) {
      ok <- is.finite(hb) & is.finite(ivb)
      hb <- hb[ok]
      ivb <- ivb[ok]
      if (length(hb) < 8) {
        return(NULL)
      }

      S <- stats::cov(cbind(hb, ivb))
      if (any(!is.finite(S))) {
        return(NULL)
      }
      if (abs(det(S)) < 1e-10) {
        return(NULL)
      }

      r2 <- stats::qchisq(level, df = 2)
      eig <- tryCatch(eigen(S), error = function(e) NULL)
      if (is.null(eig)) {
        return(NULL)
      }

      vals <- pmax(eig$values, 0)
      A <- eig$vectors %*% diag(sqrt(vals * r2), 2, 2) %*% t(eig$vectors)

      t <- seq(0, 2 * pi, length.out = n)
      circ <- rbind(cos(t), sin(t))
      mu <- c(mean(hb), mean(ivb))

      pts <- t(mu + A %*% circ)
      tibble::tibble(hb = pts[, 1], ivb = pts[, 2])
    }

    ell_df <- movement |>
      dplyr::group_by(pitch_name) |>
      dplyr::group_modify(\(d, key) {
        out <- ellipse_pts(
          d$horizontal_break,
          d$induced_vertical_break,
          level = 0.68
        )
        if (is.null(out) || nrow(out) == 0) tibble::tibble() else out
      }) |>
      dplyr::ungroup()

    if (nrow(ell_df) > 0) {
      for (pt in unique(ell_df$pitch_name)) {
        d1 <- ell_df |> dplyr::filter(pitch_name == pt)
        col_pt <- unname(pal[pt])
        if (length(col_pt) == 0 || is.na(col_pt)) {
          col_pt <- "#333333"
        }

        p <- p |>
          plotly::add_trace(
            data = d1,
            x = ~hb,
            y = ~ivb,
            type = "scatter",
            mode = "lines",
            showlegend = FALSE,
            inherit = FALSE,
            hoverinfo = "skip",
            line = list(width = 2, color = col_pt)
          )
      }
    }
  }

  x_title <- if (savant_view) {
    "Horizontal Break (in)  (1B \u2190  \u2192 3B)"
  } else {
    "Horizontal Break (in)"
  }

  p |>
    plotly::layout(
      title = "Pitch Movement",
      xaxis = list(title = x_title, zeroline = TRUE, range = c(-25, 25)),
      yaxis = list(
        title = "Induced Vertical Break (in)",
        zeroline = TRUE,
        range = c(-25, 25)
      ),
      legend = list(orientation = "h", y = -0.2),
      hovermode = "closest"
    ) |>
    plotly::config(displayModeBar = FALSE)
}

plot_pitch_velocity <- function(tbl) {
  velo <- tbl |>
    tibble::as_tibble() |>
    dplyr::filter(
      !is.na(pitch_name),
      !pitch_name %in% c("", "Pitch Out"),
      !is.na(release_speed)
    ) |>
    dplyr::mutate(
      pitch_name = forcats::fct_reorder(
        pitch_name,
        release_speed,
        .fun = mean,
        .desc = TRUE
      )
    )

  pal <- pitchTypePalette(velo$pitch_name)

  pitch_lvl <- summarise_pitch_level(velo)
  pa_end <- summarise_pa_ending(velo)

  velo_dist <- velo |>
    dplyr::group_by(pitch_name) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_v = mean(release_speed, na.rm = TRUE),
      p10_v = as.numeric(stats::quantile(release_speed, 0.10, na.rm = TRUE)),
      p90_v = as.numeric(stats::quantile(release_speed, 0.90, na.rm = TRUE)),
      y_anchor = stats::median(release_speed, na.rm = TRUE),
      .groups = "drop"
    )

  fmt_num <- function(x, digits = 1) {
    ifelse(is.na(x), "—", sprintf(paste0("%.", digits, "f"), x))
  }
  fmt_int <- function(x) ifelse(is.na(x), "—", sprintf("%d", x))
  fmt_pct <- function(x, digits = 1) {
    ifelse(is.na(x), "—", sprintf(paste0("%.", digits, "f%%"), 100 * x))
  }
  row2 <- function(k, v) sprintf("%-10s %8s", k, v)

  hover_tbl <- velo_dist |>
    dplyr::left_join(pitch_lvl, by = "pitch_name") |>
    dplyr::left_join(
      pa_end |>
        dplyr::select(pitch_name, pa, ab, bbe, ba, slg, hardhit),
      by = "pitch_name"
    ) |>
    dplyr::mutate(
      hovertext = paste(
        "<br><b>Velocity</b>",
        paste0("<br>", row2("Pitches", fmt_int(n))),
        paste0("<br>", row2("Mean", paste0(fmt_num(mean_v, 1), " mph"))),
        paste0(
          "<br>",
          row2(
            "P10–P90",
            paste0(fmt_num(p10_v, 1), "–", fmt_num(p90_v, 1), " mph")
          )
        ),

        "<br><br><b>Pitch skill</b>",
        paste0("<br>", row2("Whiff%", fmt_pct(whiff_pct, 1))),
        paste0("<br>", row2("PutAway%", fmt_pct(putaway_pct, 1))),
        paste0(
          "<br>",
          row2("Spin", ifelse(is.na(spin), "—", sprintf("%.0f rpm", spin)))
        ),
        paste0(
          "<br>",
          row2("Ext", ifelse(is.na(ext), "—", sprintf("%.1f ft", ext)))
        ),

        "<br><br><b>Results</b>",
        paste0("<br>", row2("PA", fmt_int(pa))),
        paste0("<br>", row2("BA", ifelse(is.na(ba), "—", sprintf("%.3f", ba)))),
        paste0(
          "<br>",
          row2("SLG", ifelse(is.na(slg), "—", sprintf("%.3f", slg)))
        ),
        paste0("<br>", row2("BBE", fmt_int(bbe))),
        paste0("<br>", row2("HardHit%", fmt_pct(hardhit, 1))),

        ifelse(n < 20, "<br><br><i>Small sample</i>", ""),

        sep = ""
      )
    )

  plotly::plot_ly() |>
    plotly::add_trace(
      data = velo,
      x = ~pitch_name,
      y = ~release_speed,
      type = "violin",
      color = ~pitch_name,
      colors = pal,
      box = list(visible = TRUE),
      meanline = list(visible = TRUE),
      spanmode = "hard",
      points = FALSE,
      hoveron = "points",
      hoverinfo = "skip",
      showlegend = TRUE
    ) |>
    plotly::add_trace(
      data = hover_tbl,
      x = ~pitch_name,
      y = ~y_anchor,
      type = "scatter",
      mode = "markers",
      marker = list(size = 30, color = "rgba(0,0,0,0)"),
      showlegend = FALSE,
      name = "",
      text = ~hovertext,
      hovertemplate = "%{text}<extra></extra>",
      hoverinfo = "text"
    ) |>
    plotly::layout(
      title = "Velocity by Pitch Type",
      xaxis = list(
        title = "Pitch Type",
        showspikes = FALSE
      ),
      yaxis = list(
        title = "Velocity (mph)",
        showspikes = FALSE,
        gridcolor = "rgba(0,0,0,0.07)",
        zeroline = FALSE
      ),
      legend = list(
        orientation = "h",
        y = -0.2,
        font = list(size = 11)
      ),
      hovermode = "x unified",
      hoverlabel = list(
        font = list(family = "Consolas, Menlo, Monaco, monospace", size = 13),
        bgcolor = "rgba(255,255,255,0.98)",
        bordercolor = "rgba(0,0,0,0.35)"
      )
    ) |>
    plotly::config(displayModeBar = FALSE)
}

make_field_shapes <- function(
  xr,
  yr,
  arc_frac = 0.96,
  infield_frac = 0.23,
  dirt_opacity = 0.28,
  line_col = "rgba(60,60,60,0.55)"
) {
  hx <- mean(xr)
  hy <- min(yr)

  r <- min(diff(xr) / 2, diff(yr)) * arc_frac
  b <- r * infield_frac

  # Foul lines endpoints (45 degrees visually).
  left_end <- c(hx - r, hy + r)
  right_end <- c(hx + r, hy + r)

  # Outfield arc path.
  t <- seq(pi * 0.80, pi * 0.20, length.out = 90)
  arc_x <- hx + r * cos(t)
  arc_y <- hy + r * sin(t)
  arc_path <- paste0(
    "M ",
    sprintf("%.3f", arc_x[1]),
    ",",
    sprintf("%.3f", arc_y[1]),
    paste(sprintf(" L %.3f,%.3f", arc_x[-1], arc_y[-1]), collapse = "")
  )

  # Infield dirt: a smaller arc wedge + diamond outline.
  r_dirt <- b * 2.15
  td <- seq(pi * 0.78, pi * 0.22, length.out = 70)
  dirt_x <- hx + r_dirt * cos(td)
  dirt_y <- hy + r_dirt * sin(td)

  dirt_path <- paste0(
    "M ",
    hx,
    ",",
    hy,
    paste(sprintf(" L %.3f,%.3f", dirt_x, dirt_y), collapse = ""),
    " Z"
  )

  # Base coordinates.
  home <- c(hx, hy)
  b1 <- c(hx + b, hy + b)
  b2 <- c(hx, hy + 2 * b)
  b3 <- c(hx - b, hy + b)

  base_sq <- function(p, s = b * 0.18) {
    list(
      type = "rect",
      x0 = p[1] - s,
      x1 = p[1] + s,
      y0 = p[2] - s,
      y1 = p[2] + s,
      line = list(width = 1, color = "rgba(40,40,40,0.7)"),
      fillcolor = "rgba(255,255,255,0.9)"
    )
  }

  list(
    list(
      type = "path",
      path = dirt_path,
      line = list(width = 0, color = "rgba(0,0,0,0)"),
      fillcolor = sprintf("rgba(210,180,140,%.2f)", dirt_opacity),
      layer = "below"
    ),

    # Basepaths / diamond outline.
    list(
      type = "line",
      x0 = home[1],
      y0 = home[2],
      x1 = b1[1],
      y1 = b1[2],
      line = list(width = 2, color = line_col),
      layer = "below"
    ),
    list(
      type = "line",
      x0 = home[1],
      y0 = home[2],
      x1 = b3[1],
      y1 = b3[2],
      line = list(width = 2, color = line_col),
      layer = "below"
    ),
    list(
      type = "line",
      x0 = b1[1],
      y0 = b1[2],
      x1 = b2[1],
      y1 = b2[2],
      line = list(width = 2, color = line_col),
      layer = "below"
    ),
    list(
      type = "line",
      x0 = b3[1],
      y0 = b3[2],
      x1 = b2[1],
      y1 = b2[2],
      line = list(width = 2, color = line_col),
      layer = "below"
    ),

    # Foul lines.
    list(
      type = "line",
      x0 = hx,
      y0 = hy,
      x1 = left_end[1],
      y1 = left_end[2],
      line = list(width = 2, color = line_col),
      layer = "below"
    ),
    list(
      type = "line",
      x0 = hx,
      y0 = hy,
      x1 = right_end[1],
      y1 = right_end[2],
      line = list(width = 2, color = line_col),
      layer = "below"
    ),

    # Outfield arc.
    list(
      type = "path",
      path = arc_path,
      line = list(width = 2, color = line_col),
      fillcolor = "rgba(0,0,0,0)",
      layer = "below"
    ),

    # Bases.
    base_sq(b1),
    base_sq(b2),
    base_sq(b3)
  )
}

plotContactEVAngle <- function(dt) {
  balls_in_play <- dt[!is.na(launch_speed) & !is.na(launch_angle)]

  balls_in_play[,
    event_label := data.table::fifelse(
      is.na(events) | events == "",
      "In Play",
      events
    )
  ]

  balls_in_play[, event_label := fifelse(events == "", "In Play", events)]
  balls_in_play[, event_label := str_replace_all(event_label, "_", " ")]
  balls_in_play[, event_label := str_to_title(event_label)]

  pal <- c(
    "Field Out" = "#B0B0B0",
    "Force Out" = "#8F8F8F",
    "Grounded Into Double Play" = "#6F6F6F",
    "Fielders Choice" = "#A7A7A7",
    "Field Error" = "#8BB8FF",
    "In Play" = "#5BC0EB",
    "Single" = "#74C476",
    "Double" = "#31A354",
    "Triple" = "#FD8D3C",
    "Home Run" = "#DE2D26"
  )

  lvl <- c(
    "Field Out",
    "Force Out",
    "Grounded Into Double Play",
    "Fielders Choice",
    "Field Error",
    "In Play",
    "Single",
    "Double",
    "Triple",
    "Home Run"
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
      xaxis = list(
        title = "Launch Angle (°)",
        range = c(xr[1] - xpad, xr[2] + xpad)
      ),
      yaxis = list(
        title = "Exit Velocity (mph)",
        range = c(yr[1] - ypad, yr[2] + ypad)
      ),
      legend = list(orientation = "h", y = -0.2),
      hovermode = "closest"
    ) |>
    config(displayModeBar = FALSE)
}

plotContactSprayChart <- function(dt) {
  balls_in_play <- dt[type == "X"]
  balls_in_play[, hc_y := hc_y * -1]

  balls_in_play[,
    event_label := data.table::fifelse(
      is.na(events) | events == "",
      "In Play",
      events
    )
  ]
  balls_in_play[,
    event_label := stringr::str_replace_all(event_label, "_", " ")
  ]
  balls_in_play[, event_label := stringr::str_to_title(event_label)]

  # Helper formatter for NAs
  fmt <- function(x, digits = 1, suffix = "") {
    ifelse(
      is.na(x),
      "—",
      paste0(format(round(x, digits), nsmall = digits), suffix)
    )
  }

  balls_in_play[, `:=`(
    hv_date = as.character(game_date),
    hv_ev = fmt(launch_speed, 1, " mph"),
    hv_la = fmt(launch_angle, 1, "°"),
    hv_dist = fmt(hit_distance_sc, 0, " ft"),
    hv_xwOBA = fmt(estimated_woba_using_speedangle, 3, ""),
    hv_xBA = fmt(estimated_ba_using_speedangle, 3, ""),
    hv_pitch = ifelse(is.na(pitch_name) | pitch_name == "", "—", pitch_name),
    hv_velo = fmt(release_speed, 1, " mph"),
    hv_spin = fmt(release_spin_rate, 0, " rpm"),
    hv_ext = fmt(release_extension, 1, " ft"),
    hv_bspd = fmt(bat_speed, 1, " mph"),
    hv_slen = fmt(swing_length, 1, " ft"),
    hv_aa = fmt(attack_angle, 1, "°"),
    hv_dRE = fmt(delta_run_exp, 2, "")
  )]

  balls_in_play[,
    hovertext := paste0(
      "<b>",
      as.character(event_label),
      "</b>",
      "<br>Date: ",
      hv_date,
      "<br>Pitch: ",
      hv_pitch,
      "<br><br><b>Contact</b>",
      "<br>EV / LA: ",
      hv_ev,
      " / ",
      hv_la,
      "<br>Dist: ",
      hv_dist,
      "<br>xwOBA: ",
      hv_xwOBA,
      " | xBA: ",
      hv_xBA,
      "<br><br><b>Pitch</b>",
      "<br>Velo: ",
      hv_velo,
      " | Spin: ",
      hv_spin,
      " | Ext: ",
      hv_ext,
      "<br><br><b>Swing</b>",
      "<br>Bat spd: ",
      hv_bspd,
      " | Swing len: ",
      hv_slen,
      " | Attack: ",
      hv_aa,
      "<br><br>ΔRun Expectancy: ",
      hv_dRE
    )
  ]

  pal <- c(
    "Field Out" = "#B0B0B0",
    "Force Out" = "#8F8F8F",
    "Grounded Into Double Play" = "#6F6F6F",
    "Fielders Choice" = "#A7A7A7",
    "Field Error" = "#8BB8FF",
    "In Play" = "#5BC0EB",
    "Single" = "#74C476",
    "Double" = "#31A354",
    "Triple" = "#FD8D3C",
    "Home Run" = "#DE2D26"
  )

  lvl <- c(
    "Field Out",
    "Force Out",
    "Grounded Into Double Play",
    "Fielders Choice",
    "Field Error",
    "In Play",
    "Single",
    "Double",
    "Triple",
    "Home Run"
  )
  balls_in_play[, event_label := factor(event_label, levels = lvl)]

  xr <- range(balls_in_play$hc_x, na.rm = TRUE)
  yr <- range(balls_in_play$hc_y, na.rm = TRUE)
  xpad <- diff(xr) * 0.05
  ypad <- diff(yr) * 0.05
  xr2 <- c(xr[1] - xpad, xr[2] + xpad)
  yr2 <- c(yr[1] - ypad, yr[2] + ypad)

  field_shapes <- make_field_shapes(xr2, yr2)

  plot_ly(
    data = balls_in_play,
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
    text = ~hovertext,
    hovertemplate = "%{text}<extra></extra>"
  ) |>
    layout(
      title = "Spray Chart",
      shapes = field_shapes,
      plot_bgcolor = "rgba(226,242,226,1)",
      paper_bgcolor = "rgba(226,242,226,1)",
      margin = list(l = 10, r = 10, t = 50, b = 60),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.08,
        bgcolor = "rgba(255,255,255,0.65)"
      ),
      font = list(color = "rgba(40,40,40,1)"),
      xaxis = list(
        title = "",
        range = xr2,
        fixedrange = TRUE,
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE,
        ticks = "",
        showline = FALSE
      ),
      yaxis = list(
        title = "",
        range = yr2,
        fixedrange = TRUE,
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE,
        ticks = "",
        showline = FALSE,
        scaleanchor = "x",
        scaleratio = 1
      ),
      hovermode = "closest"
    ) |>
    config(displayModeBar = FALSE)
}

plotContactEVAngleDensity <- function(dt) {
  balls_in_play <- dt[!is.na(launch_speed) & !is.na(launch_angle)]

  xr2 <- c(-80, 80)
  yr2 <- c(30, 125)

  nbin_x <- 24
  nbin_y <- 22
  bin_size_x <- diff(xr2) / nbin_x
  bin_size_y <- diff(yr2) / nbin_y

  density_max <- 0.025
  density_ticks <- seq(0, density_max, by = 0.01)

  plot_ly(
    balls_in_play,
    x = ~launch_angle,
    y = ~launch_speed,
    type = "histogram2d",
    histnorm = "probability",
    autobinx = FALSE,
    autobiny = FALSE,
    xbins = list(start = xr2[1], end = xr2[2], size = bin_size_x),
    ybins = list(start = yr2[1], end = yr2[2], size = bin_size_y),
    zauto = FALSE,
    zmin = 0,
    zmax = density_max,
    colorscale = "YlOrRd",
    reversescale = FALSE,
    colorbar = list(
      title = "% of BIP",
      tickvals = density_ticks,
      ticktext = paste0(round(density_ticks * 100), "%")
    ),
    hovertemplate = paste(
      "LA bin: %{x:.1f}°",
      "<br>EV bin: %{y:.1f} mph",
      "<br>Share of BIP: %{z:.1%}",
      "<extra></extra>"
    )
  ) |>
    layout(
      title = "EV/LA Density",
      xaxis = list(title = "Launch Angle (°)", range = xr2),
      yaxis = list(title = "Exit Velocity (mph)", range = yr2),
      plot_bgcolor = "#A50026",
      paper_bgcolor = "white",
      shapes = list(
        list(
          type = "line",
          x0 = xr2[1],
          x1 = xr2[2],
          y0 = 95,
          y1 = 95,
          line = list(color = "#00E676", dash = "dash", width = 3.5)
        ),
        list(
          type = "rect",
          x0 = 8,
          x1 = 32,
          y0 = yr2[1],
          y1 = yr2[2],
          line = list(
            color = "rgba(56, 142, 60, 0.9)",
            dash = "dot",
            width = 2.5
          ),
          fillcolor = "rgba(56, 142, 60, 0.08)"
        )
      ),
      annotations = list(
        list(
          x = -62,
          y = 95,
          text = "Hard-hit EV threshold (95 mph)",
          showarrow = FALSE,
          bgcolor = "rgba(255, 255, 255, 0.75)",
          bordercolor = "#00E676",
          borderwidth = 1.5,
          borderpad = 3,
          font = list(size = 12, color = "#004D40"),
          xanchor = "left",
          yanchor = "bottom"
        ),
        list(
          x = 20,
          y = yr2[1] + 4,
          text = "Sweet-spot LA band (8-32°)",
          showarrow = FALSE,
          bgcolor = "rgba(255, 255, 255, 0.75)",
          bordercolor = "rgba(56, 142, 60, 0.9)",
          borderwidth = 1.5,
          borderpad = 3,
          font = list(size = 12, color = "#1B5E20"),
          xanchor = "center",
          yanchor = "bottom"
        )
      )
    ) |>
    config(displayModeBar = FALSE)
}
