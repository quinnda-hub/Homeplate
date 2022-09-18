#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

GUTS <- function() {
  "https://www.fangraphs.com/guts.aspx?type=cn" |>
    rvest::read_html() |>
    rvest::html_element(xpath = "//*[(@id = \"GutsBoard1_dg1_ctl00\")]") |>
    rvest::html_table() |> as.data.table()
}
