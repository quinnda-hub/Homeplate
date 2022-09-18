standings <- standings(2022)

.gb <- function(dt) {
  div <- copy(dt)
  div <- dt[order(-frank(wins))]

  lw <- div[1, wins]
  fw <- div[2:nrow(div), wins]

  ll <- div[1, losses]
  fl <- div[2:nrow(div), losses]

  div[, gb := c("--", ((lw - ll) - (fw - fl)) / 2)][]
}

standings_tbl <- function(dt, division) {
  if (division == "al west") {
    dt <- dt[id %in% c(117, 108, 140, 136, 133)]
  } else if (division == "al central") {
    dt <- dt[id %in% c(142, 145, 114, 118, 116)]
  } else if (division == "al east") {
    dt <- dt[id %in% c(147, 139, 141, 111, 110)]
  } else if (division == "nl west") {
    dt <- dt[id %in% c(119, 135, 137, 109, 115)]
  } else if (division == "nl central") {
    dt <- dt[id %in% c(158, 138, 134, 112, 113)]
  } else if (division == "nl east") {
    dt <- dt[id %in% c(121, 144, 143, 146, 120)]
  } else {
    stop("Please input valid division")
  }

  reactable(.gb(dt), columns = list(team = colDef(cell = function(value) {
    image <- img(src = paste0("data/logos/", standings[team == value, id], ".svg"),
                 height = "24px", alt = value)
    tagList(
      div(style = list(display = "inline-block", width = "45px"), image),
      value
    )},
    name = "Team"),
    id = colDef(show = FALSE),
    wins = colDef(name = "Wins",
                  align = "center"),
    losses = colDef(name = "Losses",
                    align = "center"),
    pct = colDef(name = "PCT",
                 align = "center"),
    gb = colDef(name = "GB",
                align = "center",
                sortable = FALSE)),
    compact = TRUE,
    style = list(fontFamily = "Fira Mono"))
}
