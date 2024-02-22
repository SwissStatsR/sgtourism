#' Get years for UI
#'
#' @description Get a list of year values based on the data for the UI.
#'
#' @param data dataset with an "Jahr" variable
#'
#' @importFrom assertthat assert_that
#'
#' @return A list of vectors.
#'
#' @noRd
get_years <- function(data) {
  assertthat::assert_that("Jahr" %in% names(data), msg = "`Jahr` variable name missing from data.")
  year_choices <- sort(unique(data[["Jahr"]]), decreasing = TRUE)
  list(
    year_choices_referenz = year_choices,
    beobachtungsjahr_selected = year_choices[1],
    referenzjahr_selected = year_choices[-1][1] # most recent year can NOT be chosen as reference
  )
}
