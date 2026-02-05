# since api calls will fail when called with >831 player_ids, game_pks, etc
# this function is useful to chunk when more than that maximum is expected
chunk <- function(x, max) {
  split(x, ceiling(seq_along(x) / max))
}

# this function will download the headshot for a given player
downloadHeadshot <- function(player_id, path) {
  trimBackground <- function(url) {
    trim <- geometry_area(0, 0, 0, 10)

    url |>
      image_read() |>
      image_crop(trim) |>
      image_fill(color = "white",
                 fuzz = 9,
                 point = "+1+1")


  }

  url <- paste0("https://img.mlbstatic.com/mlb-photos/image/upload/d_people:generic:headshot:67:current.png/w_426,q_auto:best/v1/people/",
                player_id, "/headshot/67/current")

  url |> trimBackground() |> image_write(path)
}

# this function will download a logo for a given team
downloadLogo <- function(team_id) {
  paste0("https://www.mlbstatic.com/team-logos/", team_id, ".svg") |>
    download.file(paste0("data-raw/logos/", team_id, ".svg"))
}

searchList <- function(dt) {
  dt <- dt[!duplicated(player_id), .(player_id, name)]

  v <- dt$player_id

  names(v) <- dt$name

  v
}

qualified <- function(stats, stnds, group = "hitting") {
  if (group == "hitting") {
    metric <- "plate_appearances"
    x <- 3.1
  } else {
    metric <- "innings_pitched"
    x <- 1
  }

  dt <- copy(stats[season == max(season)])
  dt <- merge(dt, stnds[, .(team, team_games = wins + losses)], by = "team",
              all.x = TRUE)

  dt[get(metric) >= (team_games * x) |> floor()]
}

retry <- function(expr, isError=function(x) "try-error" %in% class(x), maxErrors=5, sleep=0) {
  attempts = 0
  retval = try(eval(expr))
  while (isError(retval)) {
    attempts = attempts + 1
    if (attempts >= maxErrors) {
      msg = sprintf("retry: too many retries [[%s]]", capture.output(str(retval)))
      flog.fatal(msg)
      stop(msg)
    } else {
      msg = sprintf("retry: error in attempt %i/%i [[%s]]", attempts, maxErrors,
                    capture.output(str(retval)))
      flog.error(msg)
      warning(msg)
    }
    if (sleep > 0) Sys.sleep(sleep)
    retval = try(eval(expr))
  }
  return(retval)
}

GUTS <- function ()
{
  tryCatch(expr = {
    guts_table <- "http://www.fangraphs.com/guts.aspx?type=cn" |>
      xml2::read_html() |>
      rvest::html_element(xpath = '//*[@id="content"]/div[3]/div[2]/div/div/div[1]/table/tbody') |>
      rvest::html_table() |> setNames(c("season", "lg_woba",
                                         "woba_scale", "wBB", "wHBP", "w1B", "w2B", "w3B",
                                         "wHR", "runSB", "runCS", "lg_r_pa", "lg_r_w", "cFIP"))
    guts_table <- guts_table |> make_baseballr_data("GUTS data from FanGraphs.com",
                                                     Sys.time())
  }, error = function(e) {
    message(glue::glue("{Sys.time()}: Invalid arguments or no GUTS data available!"))
  }, finally = {
  })
  return(as.data.table(guts_table))
}

getCurrentSeason <- function() {
  if (format(Sys.Date(), "%m") == "04") {
    format(Sys.Date(), "%Y")
  } else {
    as.integer(format(Sys.Date(), "%Y")) - 1
  }
}
