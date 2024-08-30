#' Text with clickable question icon with popover text
#'
#' @param text text to show
#' @param popover_content text content of the popover
#' @param input_id unique ID to make the popover work
#'
#' @return an text followed by a clickable icon button with a popover text.
#'
#' @importFrom bs4Dash popover actionButton
#'
#'
#' @noRd
text_with_popover_icon <- function(text, popover_content, input_id) {
  span(
    text,
    bs4Dash::popover(
      tag = bs4Dash::actionButton(
        inputId = input_id,
        label = NULL,
        icon = shiny::icon("question-circle"),
        class = "border-0 p-0"),
      title = NULL,
      content = popover_content
    )
  )
}

#' Get color palette
#'
#' @description Get St.Gallen color palettes
#'
#' @param name name of the color palette
#'
#' @return character string of HEX codes.
#'
#' @noRd
color_palette <- function(name) {
  if(name == "pal_sg_1") {
    return(c("#00953b", "#6E2585", "#014a97"))
  }
  if(name == "pal_sg_2") {
    return(c("#00a0ae", "#f18902", "#80bd00"))
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
down_arrow <- function(color = "#8a2432") {
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
get_number_color <- function(number, color_positive = "#009fe3", color_negative = "#8a2432", color_neutral = "#000000") {
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
get_number_icon <- function(number, color_positive = "#009fe3", color_negative = "#8a2432") {
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

#' A helper to add basic tooltip inside a gt table
#' SOURCE: https://github.com/jthomasmock/gtExtras/blob/master/R/html-helpers.R
#' @description This is a lightweight helper to add tooltip, typically to be
#' used within `gt::cols_label()`.
#' @param label The label for the item with a tooltip
#' @param tooltip The text based tooltip for the item
#'
#' @return HTML text
#'
#' @noRd
with_tooltip <- function(label, tooltip) {
  tags$abbr(
    style = paste0(
      "cursor: question; font-weight: bold;"
    ),
    title = tooltip, label
  ) %>%
    as.character() %>%
    gt::html()
}


#' helper to get the most recent months of the actual year
#'
#' @param df data frame for which the month has to be recognized
#'
#' @importFrom dplyr filter pull
#'
#' @return Number of most recent month
#'
#' @noRd
max_month_actual <-function(df) {
  unique(df |>
           filter(Jahr == max(Jahr)) |>
           filter(Monat == max(Monat)) |>
           pull(Monat)
         )
}

#' helper to get needed variable names for selected country
#'
#' @param country_name country name from interactive input
#'
#' @importFrom dplyr filter select
#'
#' @return variable names for arrivals and overnight stays
#'
#' @noRd
get_country_vars <- function(country_name) {
  sgtourism::meta_countries |>
    filter(Country2 == country_name) |>
    select(Country2, CountryAK, CountryLN) |>
    unlist() |>
    as.character()
}
