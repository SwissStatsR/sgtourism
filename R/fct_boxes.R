#' Get data for box tables
#'
#' Prepare data to be used in `create_box()`, `create_box_dur_stay()` and
#' `create_box_pct()`. Returns a data.frame which summarized my indicators
#' based on filtered data and months selected. For detailed breakdown by markt,
#' see `get_box_data_markt()`.
#'
#' @param data data.frame produced by `filter_data()`.
#' @param months_selected reactive vector months_selected()
#' @param input input
#'
#' @importFrom dplyr filter group_by summarise ungroup arrange mutate join_by
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom assertthat assert_that
#'
#' @return a data.frame to be used with `create_box_*()` functions.
#' @keywords internal
#' @export
get_box_data <- function(data, months_selected, input) {
  assertthat::assert_that("Monat" %in% names(data), msg = "`Monat` variable name missing from data.")
  assertthat::assert_that("Referenz" %in% names(data), msg = "`Referenz` variable name missing from data.")
  assertthat::assert_that(is.numeric(months_selected))

  data_box <- data |>
    filter(Monat %in% months_selected) |>
    group_by(Referenz) |>
    summarise(Betriebe = round(sum(Betriebe, na.rm = TRUE) / length(months_selected), 0),
              Betten = round(sum(Betten, na.rm = TRUE) / length(months_selected), 0),
              Zimmer = round(sum(Zimmer, na.rm = TRUE) / length(months_selected), 0),
              Ankuenfte = sum(Ankuenfte, na.rm = TRUE),
              Logiernaechte = sum(Logiernaechte, na.rm = TRUE),
              hot_size = round(Betten / Betriebe, 1),
              Zimmernaechte = sum(Zimmernaechte, na.rm = TRUE),
              dur_stay = sum(Logiernaechte) / sum (Ankuenfte),
              bed_occ = sum(Logiernaechte, na.rm = TRUE) / sum (Bettenmonat, na.rm = TRUE),
              room_occ = sum(Zimmernaechte, na.rm = TRUE) / sum (Zimmermonat, na.rm = TRUE),
              bed_net_occ = sum(Logiernaechte, na.rm = TRUE) / sum (NettoBett, na.rm = TRUE),
              room_net_occ = sum(Zimmernaechte, na.rm = TRUE) / sum (NettoZimm, na.rm = TRUE)
    ) |>
    ungroup() |>
    arrange(Referenz) |>
    pivot_longer(
      cols = c(Betriebe, Betten, Zimmer, Ankuenfte, Logiernaechte, hot_size,
               Zimmernaechte, dur_stay, bed_occ, bed_net_occ, room_occ, room_net_occ),
      names_to = "Indicator",
      values_to = "Abs_diff") |>
    pivot_wider(
      names_from = Referenz,
      values_from = Abs_diff
    ) |>
    mutate(diff_abs = beobachtungsjahr - referenzjahr,
           percent_change = (beobachtungsjahr - referenzjahr)/ referenzjahr * 100
    )
  return(data_box)
}

#' Get data for Markt box tables
#'
#' Data wrangling to prepare data for the boxes tables related to the "markt"
#' input, i.e. "input$markt".
#'
#' @param data data.frame produced by `filter_data()`.
#' @param months_selected reactive vector months_selected()
#' @param input input
#'
#' @importFrom dplyr filter group_by summarise ungroup arrange mutate na_if
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @return a data.frame to be used with `create_box_*()` functions.
#' @keywords internal
#' @export
get_box_data_markt <- function(data, months_selected, input) {
  assertthat::assert_that("Monat" %in% names(data), msg = "`Monat` variable name missing from data.")
  assertthat::assert_that("Jahr" %in% names(data), msg = "`Jahr` variable name missing from data.")
  assertthat::assert_that("Referenz" %in% names(data), msg = "`Referenz` variable name missing from data.")
  assertthat::assert_that(is.numeric(months_selected))

  # "LN" for Logiernaechte
  df_ln <- data |>
    filter(Monat %in% months_selected) |>
    select(Jahr, Referenz, starts_with("LN")) |>
    pivot_longer(cols = starts_with("LN"), names_to = "Country", values_to = "Value") |>
    mutate(Country = gsub("^LN","", Country)) |>
    mutate(Country = recode(Country, "AL" = "Ausland total")) |> # "AL" as "Ausland total"
    group_by(Jahr, Referenz, Country) |>
    summarise(TotalLN = sum(Value, na.rm = TRUE)) |>
    ungroup() |>
    arrange(desc(TotalLN)) |>
    select(-Jahr) |>
    pivot_wider(names_from = Referenz, values_from = TotalLN) |>
    # mutate(beobachtungsjahr = dplyr::na_if(beobachtungsjahr, 0),
    #        referenzjahr = dplyr::na_if(referenzjahr, 0)) |>
    rename("beobachtungsjahrLN" = "beobachtungsjahr", "referenzjahrLN" = "referenzjahr")

  # "AK" for Ankuenfte
  df_ak <- data |>
    filter(Monat %in% months_selected) |>
    select(Jahr, Referenz, starts_with("AK")) |>
    pivot_longer(cols = starts_with("AK"), names_to = "Country", values_to = "Value") |>
    mutate(Country = gsub("^AK","", Country)) |>
    mutate(Country = recode(Country, "AL" = "Ausland total")) |> # "AL" as "Ausland total"
    group_by(Jahr, Referenz, Country) |>
    summarise(TotalLN = sum(Value, na.rm = TRUE)) |>
    ungroup() |>
    arrange(desc(TotalLN)) |>
    select(-Jahr) |>
    pivot_wider(names_from = Referenz, values_from = TotalLN) |>
    # mutate(beobachtungsjahr = dplyr::na_if(beobachtungsjahr, 0),
    #        referenzjahr = dplyr::na_if(referenzjahr, 0)) |>
    rename("beobachtungsjahrAK" = "beobachtungsjahr", "referenzjahrAK" = "referenzjahr")

  data_box_all <- df_ln |>
    left_join(df_ak, by = "Country") |>
    mutate(dur_stay_referenz = referenzjahrLN / referenzjahrAK,
           dur_stay_beobachtung = beobachtungsjahrLN / beobachtungsjahrAK) |>
    pivot_longer(-Country, names_to = "name", values_to = "value") |>
    mutate(Indicator = dplyr::case_when(
      endsWith(name, "LN") ~ "Logiernaechte",
      endsWith(name, "AK") ~ "Ankuenfte",
      startsWith(name, "dur_stay") ~ "dur_stay")
    ) |>
    mutate(
      name = gsub(pattern = "LN$", replacement = "", x = name),
      name = gsub(pattern = "AK$", replacement = "", x = name),
      name = gsub(pattern = "^dur_stay_", replacement = "", x = name),
      name = gsub(pattern = "beobachtungsjahr", replacement = "beobachtung", x = name),
      name = gsub(pattern = "referenzjahr", replacement = "referenz", x = name)
    ) |>
    pivot_wider(names_from = "name", values_from = "value") |>
    dplyr::rename(beobachtungsjahr = beobachtung, referenzjahr = referenz) |>
    mutate(diff_abs = beobachtungsjahr - referenzjahr,
           percent_change = (beobachtungsjahr - referenzjahr)/ referenzjahr * 100
    )

  data_box <- data_box_all |>
    left_join(sgtourism::meta_countries |>
                select(Country, Country2),
              by = join_by("Country" == "Country")) |>
    filter(Country2 == input$markt) |>
    select(-Country, -Country2)

  return(data_box)
}

#' Create the titles of the boxes with a popover icon
#'
#' @param title title of the box
#' @param popover_content content of the popover
#' @param input input
#'
#' @return an html object.
#' @keywords internal
#' @export
create_box_title <- function(title, popover_content, input, input_id) {
  tags$span(
    tags$b(paste0(title, " ", input$beobachtungsjahr)),
    bs4Dash::popover(
      tag = bs4Dash::actionButton(
        inputId = input_id,
        label = NULL,
        icon = icon("question-circle"),
        class = "border-0 p-0"
      ),
      title = NULL,
      content = popover_content
    )
  )
}

#' Create boxes
#'
#' @param input input
#' @param df_box_beobachtung df_box_beobachtung() reactive data
#' @param df_box_referenz df_box_referenz() reactive data
#' @param indicator "Indicator" variable value from both datasets
#'
#' @return an HTML string.
#' @keywords internal
#' @export
create_box <- function(input, df_box_beobachtung, df_box_referenz, indicator) {
  assertthat::assert_that("Indicator" %in% names(df_box_beobachtung), msg = "`Indicator` variable name missing from df_box_beobachtung.")
  assertthat::assert_that("Indicator" %in% names(df_box_referenz), msg = "`Indicator` variable name missing from df_box_referenz.")

  df_box_beobachtung_ankuenfte <- df_box_beobachtung |>
    filter(Indicator == indicator)
  df_box_referenz_ankuenfte <- df_box_referenz |>
    filter(Indicator == indicator)

  color_beobachtung_diff_abs <- get_number_color(df_box_beobachtung_ankuenfte$diff_abs)
  color_beobachtung_percent_change <- get_number_color(df_box_beobachtung_ankuenfte$percent_change)
  color_referenz_diff_abs <- get_number_color(df_box_referenz_ankuenfte$diff_abs)
  color_referenz_percent_change <- get_number_color(df_box_referenz_ankuenfte$percent_change)

  icon_beobachtung_diff_abs <- get_number_icon(df_box_beobachtung_ankuenfte$diff_abs)
  icon_beobachtung_percent_change <- get_number_icon(df_box_beobachtung_ankuenfte$percent_change)
  icon_referenz_diff_abs <- get_number_icon(df_box_referenz_ankuenfte$diff_abs)
  icon_referenz_percent_change <- get_number_icon(df_box_referenz_ankuenfte$percent_change)

  add_plus_if_needed_beobachtung_diff_abs <- add_plus_if_needed(df_box_beobachtung_ankuenfte$diff_abs)
  add_plus_if_needed_beobachtung_percent_change<- add_plus_if_needed(df_box_beobachtung_ankuenfte$percent_change)
  add_plus_if_needed_referenz_diff_abs <- add_plus_if_needed(df_box_referenz_ankuenfte$diff_abs)
  add_plus_if_needed_referenz_percent_change<- add_plus_if_needed(df_box_referenz_ankuenfte$percent_change)

  if(is.na(df_box_beobachtung_ankuenfte$beobachtungsjahr)) {
    df_box_beobachtung_ankuenfte$beobachtungsjahr <- "--"
  }
  if(is.na(df_box_beobachtung_ankuenfte$referenzjahr)) {
    df_box_beobachtung_ankuenfte$referenzjahr <- "--"
  }
  if(is.na(df_box_beobachtung_ankuenfte$diff_abs)) {
    df_box_beobachtung_ankuenfte$diff_abs <- "--"
  }
  if(is.na(df_box_beobachtung_ankuenfte$percent_change)) {
    df_box_beobachtung_ankuenfte$percent_change <- "--"
  }
  if(is.na(df_box_referenz_ankuenfte$beobachtungsjahr)) {
    df_box_referenz_ankuenfte$beobachtungsjahr <- "--"
  }
  if(is.na(df_box_referenz_ankuenfte$referenzjahr)) {
    df_box_referenz_ankuenfte$referenzjahr <- "--"
  }
  if(is.na(df_box_referenz_ankuenfte$diff_abs)) {
    df_box_referenz_ankuenfte$diff_abs <- "--"
  }
  if(is.na(df_box_referenz_ankuenfte$percent_change)) {
    df_box_referenz_ankuenfte$percent_change <- "--"
  }

  paste0("<span style='font-size: 1.1em;'>",
         input$beobachtungsregion,
         "</span>",
         "<br>",
         "<b style='font-size: 1.2em;'>",
         prettyNum(df_box_beobachtung_ankuenfte$beobachtungsjahr, big.mark = "'"),
         "</b>",
         "<br>",
         "<b style='color:", color_beobachtung_diff_abs,"';>",
         add_plus_if_needed_beobachtung_diff_abs,
         prettyNum(df_box_beobachtung_ankuenfte$diff_abs, big.mark = "'"),
         " ", icon_beobachtung_diff_abs,
         "</b>",
         "<br>",
         "<b style='color:", color_beobachtung_percent_change,"';>",
         add_plus_if_needed_beobachtung_percent_change,
         formatC(df_box_beobachtung_ankuenfte$percent_change, digits = 1, format = "f"), "%",
         " ", icon_beobachtung_percent_change,
         "</b>",
         "<br>",
         "<br>",
         "<span style='font-size: 1.1em;'>", input$referenzregion, "</span>",
         "<br>",
         "<b style='font-size: 1.2em;'>",
         prettyNum(df_box_referenz_ankuenfte$beobachtungsjahr, big.mark = "'"),
         "</b>",
         "<br>",
         "<b style='color:", color_referenz_diff_abs,"';>",
         add_plus_if_needed_referenz_diff_abs,
         prettyNum(df_box_referenz_ankuenfte$diff_abs, big.mark = "'"),
         " ", icon_referenz_diff_abs,
         "</b>",
         "<br>",
         "<b style='color:", color_referenz_percent_change,"';>",
         add_plus_if_needed_referenz_percent_change,
         formatC(df_box_referenz_ankuenfte$percent_change, digits = 1, format = "f"), "%",
         " ", icon_referenz_percent_change,
         "</b>",
         "<br>",
         "<br>",
         "<i>",
         "Ver\u00e4nderung gg\u00fc. ", input$referenzjahr,
         "</i>"
  )
}

#' Create box for "dur_stay" indicator
#'
#' @param input input
#' @param df_box_beobachtung df_box_beobachtung() reactive data
#' @param df_box_referenz df_box_referenz() reactive data
#' @param indicator Indicator "dur_stay" value from both datasets
#'
#' @return an HTML string.
#' @keywords internal
#' @export
create_box_dur_stay <- function(input, df_box_beobachtung, df_box_referenz, indicator) {
  assertthat::assert_that("Indicator" %in% names(df_box_beobachtung), msg = "`Indicator` variable name missing from df_box_beobachtung.")
  assertthat::assert_that("Indicator" %in% names(df_box_referenz), msg = "`Indicator` variable name missing from df_box_referenz.")

  df_box_beobachtung_dur_stay <- df_box_beobachtung |>
    filter(Indicator == indicator)
  df_box_referenz_dur_stay <- df_box_referenz |>
    filter(Indicator == indicator)

  color_beobachtung_diff_abs <- get_number_color(df_box_beobachtung_dur_stay$diff_abs)
  color_beobachtung_percent_change <- get_number_color(df_box_beobachtung_dur_stay$percent_change)
  color_referenz_diff_abs <- get_number_color(df_box_referenz_dur_stay$diff_abs)
  color_referenz_percent_change <- get_number_color(df_box_referenz_dur_stay$percent_change)

  icon_beobachtung_diff_abs <- get_number_icon(df_box_beobachtung_dur_stay$diff_abs)
  icon_beobachtung_percent_change <- get_number_icon(df_box_beobachtung_dur_stay$percent_change)
  icon_referenz_diff_abs <- get_number_icon(df_box_referenz_dur_stay$diff_abs)
  icon_referenz_percent_change <- get_number_icon(df_box_referenz_dur_stay$percent_change)

  add_plus_if_needed_beobachtung_diff_abs <- add_plus_if_needed(df_box_beobachtung_dur_stay$diff_abs)
  add_plus_if_needed_beobachtung_percent_change<- add_plus_if_needed(df_box_beobachtung_dur_stay$percent_change)
  add_plus_if_needed_referenz_diff_abs <- add_plus_if_needed(df_box_referenz_dur_stay$diff_abs)
  add_plus_if_needed_referenz_percent_change<- add_plus_if_needed(df_box_referenz_dur_stay$percent_change)

  if(is.na(df_box_beobachtung_dur_stay$beobachtungsjahr)) {
    df_box_beobachtung_dur_stay$beobachtungsjahr <- "--"
  }
  if(is.na(df_box_beobachtung_dur_stay$referenzjahr)) {
    df_box_beobachtung_dur_stay$referenzjahr <- "--"
  }
  if(is.na(df_box_beobachtung_dur_stay$diff_abs)) {
    df_box_beobachtung_dur_stay$diff_abs <- "--"
  }
  if(is.na(df_box_beobachtung_dur_stay$percent_change)) {
    df_box_beobachtung_dur_stay$percent_change <- "--"
  }
  if(is.na(df_box_referenz_dur_stay$beobachtungsjahr)) {
    df_box_referenz_dur_stay$beobachtungsjahr <- "--"
  }
  if(is.na(df_box_referenz_dur_stay$referenzjahr)) {
    df_box_referenz_dur_stay$referenzjahr <- "--"
  }
  if(is.na(df_box_referenz_dur_stay$diff_abs)) {
    df_box_referenz_dur_stay$diff_abs <- "--"
  }
  if(is.na(df_box_referenz_dur_stay$percent_change)) {
    df_box_referenz_dur_stay$percent_change <- "--"
  }

  paste0("<span style='font-size: 1.1em;'>",
         input$beobachtungsregion,
         "</span>",
         "<br>",
         "<b style='font-size: 1.2em;'>",
         formatC(df_box_beobachtung_dur_stay$beobachtungsjahr, digits = 2, format = "f"),
         ifelse(df_box_beobachtung_dur_stay$beobachtungsjahr > 1, " Tage", " Tag"),
         "</b>",
         "<br>",
         "<b style='color:", color_beobachtung_diff_abs, "';>",
         add_plus_if_needed_beobachtung_diff_abs,
         formatC(df_box_beobachtung_dur_stay$diff_abs, digits = 2, format = "f"),
         " ", icon_beobachtung_diff_abs,
         "</b>",
         "<br>",
         "<b style='color:", color_beobachtung_percent_change, "';>",
         add_plus_if_needed_beobachtung_percent_change,
         formatC(df_box_beobachtung_dur_stay$percent_change, digits = 1, format = "f"), "%",
         " ", icon_beobachtung_percent_change,
         "</b>",
         "<br>",
         "<br>",
         "<span style='font-size: 1.1em;'>", input$referenzregion, "</span>",
         "<br>",
         "<b style='font-size: 1.2em;'>",
         formatC(df_box_referenz_dur_stay$beobachtungsjahr, digits = 2, format = "f"),
         ifelse(df_box_referenz_dur_stay$beobachtungsjahr > 1, " Tage", " Tag"),
         "</b>",
         "<br>",
         "<b style='color:", color_referenz_diff_abs, "';>",
         add_plus_if_needed_referenz_diff_abs,
         formatC(df_box_referenz_dur_stay$diff_abs, digits = 2, format = "f"),
         " ", icon_referenz_diff_abs,
         "</b>",
         "<br>",
         "<b style='color:", color_referenz_percent_change, "';>",
         add_plus_if_needed_referenz_percent_change,
         formatC(df_box_referenz_dur_stay$percent_change, digits = 1, format = "f"), "%",
         " ", icon_referenz_percent_change,
         "</b>",
         "<br>",
         "<br>",
         "<i>",
         "Ver\u00e4nderung gg\u00fc. ", input$referenzjahr,
         "</i>"
  )
}

#' Create box for indicator with percent points
#'
#' @param input input
#' @param df_box_beobachtung df_box_beobachtung() reactive data
#' @param df_box_referenz df_box_referenz() reactive data
#' @param indicator Indicator "bed_occ" value from both datasets
#'
#' @return an HTML string.
#' @keywords internal
#' @export
create_box_pct <- function(input, df_box_beobachtung, df_box_referenz, indicator) {
  assertthat::assert_that("Indicator" %in% names(df_box_beobachtung), msg = "`Indicator` variable name missing from df_box_beobachtung.")
  assertthat::assert_that("Indicator" %in% names(df_box_referenz), msg = "`Indicator` variable name missing from df_box_referenz.")

  df_box_beobachtung_bed_occ <- df_box_beobachtung |>
    filter(Indicator == indicator)
  df_box_referenz_bed_occ <- df_box_referenz |>
    filter(Indicator == indicator)

  color_beobachtung_diff_abs <- get_number_color(df_box_beobachtung_bed_occ$beobachtungsjahr - df_box_beobachtung_bed_occ$referenzjahr)
  color_referenz_diff_abs <- get_number_color(df_box_referenz_bed_occ$beobachtungsjahr - df_box_referenz_bed_occ$referenzjahr)

  icon_beobachtung_diff_abs <- get_number_icon(df_box_beobachtung_bed_occ$beobachtungsjahr - df_box_beobachtung_bed_occ$referenzjahr)
  icon_referenz_diff_abs <- get_number_icon(df_box_referenz_bed_occ$beobachtungsjahr - df_box_referenz_bed_occ$referenzjahr)

  add_plus_if_needed_beobachtung_diff_abs <- add_plus_if_needed(df_box_beobachtung_bed_occ$beobachtungsjahr - df_box_beobachtung_bed_occ$referenzjahr)
  add_plus_if_needed_referenz_diff_abs <- add_plus_if_needed(df_box_referenz_bed_occ$beobachtungsjahr - df_box_referenz_bed_occ$referenzjahr)

  if(is.na(df_box_beobachtung_bed_occ$beobachtungsjahr)) {
    df_box_beobachtung_bed_occ$beobachtungsjahr <- "--"
  }
  if(is.na(df_box_beobachtung_bed_occ$referenzjahr)) {
    df_box_beobachtung_bed_occ$referenzjahr <- "--"
  }
  if(is.na(df_box_beobachtung_bed_occ$diff_abs)) {
    df_box_beobachtung_bed_occ$diff_abs <- "--"
  }
  if(is.na(df_box_beobachtung_bed_occ$percent_change)) {
    df_box_beobachtung_bed_occ$percent_change <- "--"
  }
  if(is.na(df_box_referenz_bed_occ$beobachtungsjahr)) {
    df_box_referenz_bed_occ$beobachtungsjahr <- "--"
  }
  if(is.na(df_box_referenz_bed_occ$referenzjahr)) {
    df_box_referenz_bed_occ$referenzjahr <- "--"
  }
  if(is.na(df_box_referenz_bed_occ$diff_abs)) {
    df_box_referenz_bed_occ$diff_abs <- "--"
  }
  if(is.na(df_box_referenz_bed_occ$percent_change)) {
    df_box_referenz_bed_occ$percent_change <- "--"
  }

  paste0("<span style='font-size: 1em;'>",
         input$beobachtungsregion,
         "</span>",
         "<br>",
         "<b style='font-size: 1.2em;'>",
         formatC(df_box_beobachtung_bed_occ$beobachtungsjahr * 100, digits = 1, format = "f"), "%",
         "</b>",
         "<br>",
         "<b style='color:", color_beobachtung_diff_abs, "';>",
         add_plus_if_needed_beobachtung_diff_abs,
         formatC((df_box_beobachtung_bed_occ$beobachtungsjahr - df_box_beobachtung_bed_occ$referenzjahr) * 100, digits = 1, format = "f"), "%-Pkt",
         " ", icon_beobachtung_diff_abs,
         "</b>",
         "<br>",
         "<br>",
         "<br>",
         "<span style='font-size: 1.1em;'>", input$referenzregion, "</span>",
         "<br>",
         "<b style='font-size: 1.2em;'>",
         formatC(df_box_referenz_bed_occ$beobachtungsjahr * 100, digits = 1, format = "f"), "%",
         "</b>",
         "<br>",
         "<b style='color:", color_referenz_diff_abs, "';>",
         add_plus_if_needed_referenz_diff_abs,
         formatC((df_box_referenz_bed_occ$beobachtungsjahr - df_box_referenz_bed_occ$referenzjahr) * 100, digits = 1, format = "f"), "%-Pkt",
         " ", icon_referenz_diff_abs,
         "</b>",
         "<br>",
         "<br>",
         "<br>",
         "<i>",
         "Ver\u00e4nderung gg\u00fc. ", input$referenzjahr,
         "</i>"
  )
}
