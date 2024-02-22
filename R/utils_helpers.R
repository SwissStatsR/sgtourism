#' Get color palette
#'
#' @description Get official St.Gallen intensiv and rein color palette
#'
#' @param name name of the color palette
#'
#' @return character string of HEX codes.
#'
#' @noRd
color_palette <- function(name) {
  if(name == "intensiv") {
    return(c("#7AB800", "#6E2585", "#0039A6"))
  }
  if(name == "rein") {
    return(c("#FCD900", "#D10074", "#009FDA"))
  }
}
#' Arrow up
#'
#' @description Create an html arrow up
#'
#' @param color Hex color of the arrow
#'
#' @return HTML arrow up.
#'
#' @noRd
up_arrow <- function(color = "#009fe3") {
  paste0("<span style=\"color:", color, "\">&#9650;</span>")
}
#' Arrow down
#'
#' @description Create an html arrow down
#'
#' @param color Hex color of the arrow
#'
#' @return HTML arrow down.
#'
#' @noRd
down_arrow <- function(color = "#cd0e2d") {
  paste0("<span style=\"color:", color, "\">&#9660;</span>")
}
#' Get color relative to number
#'
#' @description Logic to return an hex color code. Returns black if number is 0 or is not a number.
#'
#' @param number A number
#' @param color_positive Color of the positive number
#' @param color_negative Color of the negative number
#' @param color_neutral Color of the neutral number
#'
#' @return An color hex code.
#'
#' @noRd
get_number_color <- function(number, color_positive = "#009fe3", color_negative = "#cd0e2d", color_neutral = "#000000") {
  if (number > 0) {
    color_positive
  } else if (number < 0) {
    color_negative
  } else {
    color_neutral
  }
}
#' Get arrow up or down relative to number
#'
#' @description Logic to return an hex color code. If number is 0 or if not a number, returns an empty string.
#'
#' @param number A number
#' @param color_positive Color of the positive number
#' @param color_negative Color of the negative number
#' @param color_neutral Color of the neutral number
#'
#' @importFrom shiny icon
#'
#' @return An arrow up or arrow down HTML string.
#'
#' @noRd
get_number_icon <- function(number, color_positive = "#009fe3", color_negative = "#cd0e2d") {
  if (number > 0) {
    shiny::icon("caret-up", style = paste0("color: ", color_positive))
  } else if (number < 0) {
    shiny::icon("caret-down", style = paste0("color: ", color_negative))
  } else {
    ""
  }
}

#' Add "+" string if positive number
#'
#' @description Logic to return an "+" string if positive number.
#'
#' @param number A number
#'
#' @return An string.
#'
#' @noRd
add_plus_if_needed <- function(number) {
  if (number > 0) {
    "+"
  } else {
    ""
  }
}
#' Get German month abbreviations
#'
#' @description Get a string containing the abbreviations of German months.
#'
#' @return An string.
#'
#' @noRd
month_abb_de <- function() {
  c("Jan", "Feb", "M\u00e4r", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")
}

#' Customized padding for tabPanel
#'
#' @description Change default tabPanel with Bootstrap classes.
#'
#' @importFrom shiny tabPanel
#'
#' @return An string.
#'
#' @noRd
tab <- function(...) {
  shiny::tabPanel(..., class = "p-3 border border-top-0 rounded-bottom")
}
