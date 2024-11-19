#' Create Beobachtungsmonate month dropdown input UI
#'
#' Create the month dropdown using `shinyWidgets::airDatepickerInput()` based on
#' the beobachtung data and "beobachtungsjahr" input.
#'
#' @param data Reactive dataset
#' @param input input
#' @param ns ns
#' @param label_text label of the month UI
#'
#' @importFrom dplyr filter distinct arrange pull
#' @importFrom shinyWidgets airDatepickerInput
#'
#' @return a shinyWidgets airDatepickerInput input.
#' @keywords internal
#' @export
create_months_ui <- function(data, input, ns, label_text) {
  beobachtungsjahr_unique_month <- data |>
    dplyr::filter(Referenz == "beobachtungsjahr") |>
    dplyr::distinct(Jahr, Monat) |>
    dplyr::arrange(Monat) |>
    dplyr::pull(Monat)

  startmonat_selected <- as.Date(paste(input$beobachtungsjahr, beobachtungsjahr_unique_month[1], "1", sep = "-"))
  endmonat_selected <- as.Date(paste(input$beobachtungsjahr, sort(beobachtungsjahr_unique_month, decreasing = TRUE)[1], "1", sep = "-"))

  shinyWidgets::airDatepickerInput(
    inputId = ns("monat"),
    language = "de",
    label = label_text,
    value = c(startmonat_selected, endmonat_selected),
    minDate = startmonat_selected,
    maxDate = endmonat_selected,
    view = "months",
    minView = "months",
    dateFormat = "MMMM",
    toggleSelected = TRUE, # When TRUE, in range mode, it's not possible to select the same date as start and end.
    range = TRUE,
    addon = "none"
  )
}

#' Create numeric month vector based on monat input
#'
#' Extract the month as numeric from the input$monat date character and returns
#' a vector of all months between the two months selected (if multiple selection)
#' or, if single selection, return a single month as number.
#'
#' @param input monat input
#'
#' @importFrom lubridate month
#'
#' @return The return value, if any, from executing the function.
#' @keywords internal
#' @export
select_months <- function(input) {
  if (length(input$monat) == 2) {
    c(lubridate::month(input$monat[1]) : lubridate::month(input$monat[2]))
  } else if (length(input$monat) == 1) {
    c(lubridate::month(input$monat))
  }
}
