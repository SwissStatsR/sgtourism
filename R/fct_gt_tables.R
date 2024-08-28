#' Create title for gt table
#'
#' @param input input
#' @param input_region input region, either input$beobachtungsjahr or input$beobachtungsjahr
#'
#' @return an html object
#'
#' @noRd
create_gt_title <- function(input, input_region) {
  tags$div(tags$b(paste0(input_region, ", ", input$beobachtungsjahr)), tags$small(paste0(" (Ver\u00e4nderung gg\u00fc. ", input$referenzjahr, ")")))
}

#' Create data for gt table beobachtungsregion
#'
#' @param df_beobachtungsregion df_beobachtungsregion
#' @param df_box_beobachtung df_box_beobachtung
#' @param months_selected months_selected
#'
#' @importFrom tidyr pivot_longer pivot_wider complete nesting
#' @importFrom dplyr select filter mutate bind_rows distinct arrange pull group_by ungroup summarise left_join rename recode starts_with desc
#'
#' @return a dataframe.
#'
#' @noRd
create_gt_data_beobachtung <- function(df_beobachtungsregion, df_box_beobachtung, months_selected) {
  df_eda_ln <- df_beobachtungsregion |>
    filter(Monat %in% months_selected) |>
    select(Jahr, Referenz, starts_with("LN")) |>
    pivot_longer(cols = starts_with("LN"), names_to = "Country", values_to = "Value") |>
    mutate(Country = gsub("^LN","", Country)) |>
    filter(Country != "AL") |> # REMOVE "AL" COUNTRY CATEGORY
    group_by(Jahr, Referenz, Country) |>
    summarise(TotalLN = sum(Value, na.rm = TRUE)) |>
    ungroup() |>
    arrange(desc(TotalLN)) |>
    select(-Jahr) |>
    pivot_wider(names_from = Referenz, values_from = TotalLN) |>
    mutate(percent_changeLN = (beobachtungsjahr - referenzjahr) / referenzjahr) |>
    rename("beobachtungsjahrLN" = "beobachtungsjahr", "referenzjahrLN" = "referenzjahr")

  df_eda_ak <- df_beobachtungsregion |>
    filter(Monat %in% months_selected) |>
    select(Jahr, Referenz, starts_with("AK")) |>
    pivot_longer(cols = starts_with("AK"), names_to = "Country", values_to = "Value") |>
    mutate(Country = gsub("^AK","", Country)) |>
    group_by(Jahr, Referenz, Country) |>
    summarise(TotalLN = sum(Value, na.rm = TRUE)) |>
    ungroup() |>
    arrange(desc(TotalLN)) |>
    select(-Jahr) |>
    pivot_wider(names_from = Referenz, values_from = TotalLN) |>
    rename("beobachtungsjahrAK" = "beobachtungsjahr", "referenzjahrAK" = "referenzjahr")

  # get logiernaechte total value calculated for the box
  val_beobachtungsjahr_logiernaechte <- df_box_beobachtung |>
    filter(Indicator == "Logiernaechte") |>
    pull(beobachtungsjahr)

  val_referenzjahr_logiernaechte <- df_box_beobachtung |>
    filter(Indicator == "Logiernaechte") |>
    pull(referenzjahr)

  df_joined <- df_eda_ln |>
    left_join(df_eda_ak, by = "Country") |>
    mutate(dur_stay_referenz = referenzjahrLN / referenzjahrAK,
           dur_stay_beobachtung = beobachtungsjahrLN / beobachtungsjahrAK) |>
    mutate(dur_stay_diff_abs = dur_stay_beobachtung - dur_stay_referenz) |>
    mutate(beobachtung_logiernaechte = val_beobachtungsjahr_logiernaechte,
           referenz_logiernaechte = val_referenzjahr_logiernaechte) |> # add logiernaechte total value
    mutate(marktanteil = beobachtungsjahrLN / beobachtung_logiernaechte) |>
    mutate(marktanteil_referenz = referenzjahrLN / referenz_logiernaechte) |>
    mutate(marktanteil_diff = marktanteil - marktanteil_referenz) |>
    select(Country, beobachtungsjahrLN, percent_changeLN, dur_stay_beobachtung, dur_stay_diff_abs, marktanteil, marktanteil_diff)

  df_joined |>
    filter(beobachtungsjahrLN != 0) |>
    dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) |> # Infinite as NA
    arrange(desc(marktanteil))
}

#' Create data for gt table referenzregion
#'
#' @param df_referenzregion df_referenzregion
#' @param df_box_referenz df_box_referenz
#' @param months_selected months_selected
#' @param gt_data_beobachtung gt_data_beobachtung
#'
#' @importFrom dplyr select filter mutate bind_rows distinct arrange pull group_by ungroup summarise left_join rename recode starts_with desc
#'
#' @return a dataframe.
#'
#' @noRd
create_gt_data_referenzregion <- function(df_referenzregion, df_box_referenz, months_selected, gt_data_beobachtung) {
  df_eda_ln <- df_referenzregion |>
    filter(Monat %in% months_selected) |>
    select(Jahr, Referenz, starts_with("LN")) |>
    pivot_longer(cols = starts_with("LN"), names_to = "Country", values_to = "Value") |>
    mutate(Country = gsub("^LN","", Country)) |>
    filter(Country != "AL") |> # REMOVE "AL" COUNTRY CATEGORY
    group_by(Jahr, Referenz, Country) |>
    summarise(TotalLN = sum(Value, na.rm = TRUE)) |>
    ungroup() |>
    arrange(desc(TotalLN)) |>
    select(-Jahr) |>
    pivot_wider(names_from = Referenz, values_from = TotalLN) |>
    mutate(percent_changeLN = (beobachtungsjahr - referenzjahr) / referenzjahr) |>
    rename("beobachtungsjahrLN" = "beobachtungsjahr", "referenzjahrLN" = "referenzjahr")

  df_eda_ak <- df_referenzregion |>
    filter(Monat %in% months_selected) |>
    select(Jahr, Referenz, starts_with("AK")) |>
    pivot_longer(cols = starts_with("AK"), names_to = "Country", values_to = "Value") |>
    mutate(Country = gsub("^AK","", Country)) |>
    group_by(Jahr, Referenz, Country) |>
    summarise(TotalLN = sum(Value, na.rm = TRUE)) |>
    ungroup() |>
    arrange(desc(TotalLN)) |>
    select(-Jahr) |>
    pivot_wider(names_from = Referenz, values_from = TotalLN) |>
    rename("beobachtungsjahrAK" = "beobachtungsjahr", "referenzjahrAK" = "referenzjahr")

  # get logiernaechte total value calculated for the box
  val_beobachtungsjahr_logiernaechte <- df_box_referenz |>
    filter(Indicator == "Logiernaechte") |>
    pull(beobachtungsjahr)

  val_referenzjahr_logiernaechte <- df_box_referenz |>
    filter(Indicator == "Logiernaechte") |>
    pull(referenzjahr)

  df_joined <- df_eda_ln |>
    left_join(df_eda_ak, by = "Country") |>
    mutate(dur_stay_referenz = referenzjahrLN / referenzjahrAK,
           dur_stay_beobachtung = beobachtungsjahrLN / beobachtungsjahrAK) |>
    mutate(dur_stay_diff_abs = dur_stay_beobachtung - dur_stay_referenz) |>
    mutate(beobachtung_logiernaechte = val_beobachtungsjahr_logiernaechte,
           referenz_logiernaechte = val_referenzjahr_logiernaechte) |> # add logiernaechte total value
    mutate(marktanteil = beobachtungsjahrLN / beobachtung_logiernaechte) |>
    mutate(marktanteil_referenz = referenzjahrLN / referenz_logiernaechte) |>
    mutate(marktanteil_diff = marktanteil - marktanteil_referenz) |>
    select(Country, beobachtungsjahrLN, percent_changeLN, dur_stay_beobachtung, dur_stay_diff_abs, marktanteil, marktanteil_diff)

  df_cleaned <- df_joined |>
    filter(beobachtungsjahrLN != 0) |>
    dplyr::mutate_if(is.numeric, list(~dplyr::na_if(., Inf))) |> # Infinite as NA
    arrange(desc(marktanteil))

  # reorder country based on order of gt_data_beobachtung
  country_order <- gt_data_beobachtung |> arrange(desc(marktanteil)) |> pull(Country)
  df_reordered <- df_cleaned[match(country_order, df_cleaned$Country),]
  df_reordered
}


#' Create gt table beobachtungsregion
#'
#' @param data data
#' @param input input
#'
#' @importFrom downloadthis download_this
#' @importFrom gt gt md fmt_number cols_merge cols_label text_transform cells_body opt_interactive sub_missing fmt_percent tab_source_note
#'
#' @return a gt object
#'
#' @noRd
create_gt_beobachtungsregion <- function(data, input) {
  df <- data |>
    mutate(Country = gsub("([a-z])([A-Z])","\\1 \\2", Country)) |> # Space before capital letter
    mutate(Country = gsub("\\.",", ", Country)) |> # Dot replace by a comma and a blank
    mutate(Country = gsub("_1","-", Country)) |> # "_1" replace by a dash
    mutate(Country = gsub("_2","- ", Country)) # "_1" replace by a dash and blank

  df |>
    gt(locale = "de_CH") |>
    fmt_number(beobachtungsjahrLN, decimals = 0) |>
    fmt_percent(columns = c(percent_changeLN, marktanteil, marktanteil_diff), decimals = 1, drop_trailing_zeros = TRUE) |>
    fmt_number(columns =  c(dur_stay_beobachtung, dur_stay_diff_abs), decimals = 2) |>
    sub_missing(
      missing_text = "n.v."
    ) |>
    cols_merge(
      columns = c(beobachtungsjahrLN, percent_changeLN),
      pattern = paste0("<b style='font-size: 1.05em;'>{1}</b><br>{2}")
    ) |>
    cols_merge(
      columns = c(dur_stay_beobachtung, dur_stay_diff_abs),
      pattern = paste0("<b style='font-size: 1.05em;'>{1}</b><br>{2}")
    ) |>
    cols_merge(
      columns = c(marktanteil, marktanteil_diff),
      pattern = paste0("<b style='font-size: 1.05em;'>{1}</b><br>{2}-Pkt")
    ) |>
    cols_label(
      Country = with_tooltip("Herkunftsland", "Land des st\u00e4ndigen Wohnsitzes des Gastes. Dieses Land muss nicht der Nationalit\u00e4t des Gastes entsprechen."),
      beobachtungsjahrLN = md("**Logiern\u00e4chte**"),
      dur_stay_beobachtung = md("**Aufenthaltsdauer in Tagen**"),
      marktanteil = with_tooltip("Marktanteil", "Anteil der Logiern\u00e4chte von G\u00e4sten aus diesem Herkunftsland am Total der Logiern\u00e4chte")
    ) |>
    text_transform(
      locations = cells_body(
        columns = beobachtungsjahrLN,
        rows = percent_changeLN > 0 & round(percent_changeLN, 3) != 0
      ),
      fn = function(x) paste(x, up_arrow())
    ) |>
    text_transform(
      locations = cells_body(
        columns = beobachtungsjahrLN,
        rows = percent_changeLN < 0 & round(percent_changeLN, 3) != 0
      ),
      fn = function(x) paste(x, down_arrow())
    ) |>
    text_transform(
      locations = cells_body(
        columns = dur_stay_beobachtung,
        rows = dur_stay_diff_abs > 0 & round(dur_stay_diff_abs, 3) != 0
      ),
      fn = function(x) paste(x, up_arrow())
    ) |>
    text_transform(
      locations = cells_body(
        columns = dur_stay_beobachtung,
        rows = dur_stay_diff_abs < 0 & round(dur_stay_diff_abs, 3) != 0
      ),
      fn = function(x) paste(x, down_arrow())
    ) |>
    text_transform(
      locations = cells_body(
        columns = marktanteil,
        rows = marktanteil_diff > 0 & round(marktanteil_diff, 3) != 0
      ),
      fn = function(x) paste(x, up_arrow())
    ) |>
    text_transform(
      locations = cells_body(
        columns = marktanteil,
        rows = marktanteil_diff < 0 & round(marktanteil_diff, 3) != 0
      ),
      fn = function(x) paste(x, down_arrow())
    ) |>
    tab_source_note(
      df |>
        rename(
          "Herkunftsland" = Country,
          "Logiern\u00e4chte" = beobachtungsjahrLN,
          "Logiern\u00e4chte_Diff" = percent_changeLN,
          "Aufenthaltsdauer_in_Tagen" = dur_stay_beobachtung,
          "Aufenthaltsdauer_in_Tagen_Diff" = dur_stay_diff_abs,
          "Marktanteil" = marktanteil,
          "Marktanteil_Diff" = marktanteil_diff
        ) |>
        downloadthis::download_this(
          output_name = paste0(input$beobachtungsregion, "_", input$beobachtungsjahr, "_", input$referenzjahr),
          output_extension = ".xlsx", # Excel output
          button_label = "Vollst\u00e4ndige Tabelle herunterladen",
          button_type = "primary"
        )
    ) |>
    opt_interactive(
      use_search = TRUE,
      use_resizers = TRUE,
      use_highlight = TRUE,
      use_compact_mode = TRUE,
      use_text_wrapping = TRUE,
      use_page_size_select = TRUE,
      page_size_values = c(5, 10, 25, 50),
      page_size_default = 5
    )
}

#' Create gt table for referenzregion
#'
#' @param gt_table_data_referenz gt_table_data_referenz
#' @param gt_data_beobachtung gt_data_beobachtung
#' @param input input
#'
#' @importFrom dplyr mutate
#' @importFrom downloadthis download_this
#' @importFrom gt gt md fmt_number cols_merge cols_label text_transform cells_body opt_interactive sub_missing fmt_percent tab_source_note
#'
#' @return a gt object.
#'
#' @noRd
create_gt_referenzregion <- function(gt_table_data_referenz, gt_data_beobachtung, input) {

  df <- gt_table_data_referenz
  country_order <- gt_data_beobachtung |> arrange(desc(marktanteil)) |> pull(Country)
  df <- df[match(country_order, df$Country),]

  df <- df |>
    mutate(Country = gsub("([a-z])([A-Z])","\\1 \\2", Country)) |> # Space before capital letter
    mutate(Country = gsub("\\.",", ", Country)) |> # Dot replace by a comma
    mutate(Country = gsub("_1","-", Country)) |> # "_1" replace by a dash
    mutate(Country = gsub("_2","- ", Country)) # "_1" replace by a dash and blank

  df |>
    gt(locale = "de_CH") |>
    fmt_number(beobachtungsjahrLN, decimals = 0) |>
    fmt_percent(columns = c(percent_changeLN, marktanteil, marktanteil_diff), decimals = 1, drop_trailing_zeros = TRUE) |>
    fmt_number(columns =  c(dur_stay_beobachtung, dur_stay_diff_abs), decimals = 2) |>
    sub_missing(
      missing_text = "n.v."
    ) |>
    cols_merge(
      columns = c(beobachtungsjahrLN, percent_changeLN),
      pattern = paste0("<b style='font-size: 1.05em;'>{1}</b><br>{2}")
    ) |>
    cols_merge(
      columns = c(dur_stay_beobachtung, dur_stay_diff_abs),
      pattern = paste0("<b style='font-size: 1.05em;'>{1}</b><br>{2}")
    ) |>
    cols_merge(
      columns = c(marktanteil, marktanteil_diff),
      pattern = paste0("<b style='font-size: 1.05em;'>{1}</b><br>{2}-Pkt")
    ) |>
    cols_label(
      Country = md("**Herkunftsland**"),
      beobachtungsjahrLN = md("**Logiern\u00e4chte**"),
      dur_stay_beobachtung = md("**Aufenthaltsdauer in Tagen**"),
      marktanteil = md("**Marktanteil**")
    ) |>
    text_transform(
      locations = cells_body(
        columns = beobachtungsjahrLN,
        rows = percent_changeLN > 0 & round(percent_changeLN, 3) != 0
      ),
      fn = function(x) paste(x, up_arrow())
    ) |>
    text_transform(
      locations = cells_body(
        columns = beobachtungsjahrLN,
        rows = percent_changeLN < 0 & round(percent_changeLN, 3) != 0
      ),
      fn = function(x) paste(x, down_arrow())
    ) |>
    text_transform(
      locations = cells_body(
        columns = dur_stay_beobachtung,
        rows = dur_stay_diff_abs > 0 & round(dur_stay_diff_abs, 3) != 0
      ),
      fn = function(x) paste(x, up_arrow())
    ) |>
    text_transform(
      locations = cells_body(
        columns = dur_stay_beobachtung,
        rows = dur_stay_diff_abs < 0 & round(dur_stay_diff_abs, 3) != 0
      ),
      fn = function(x) paste(x, down_arrow())
    ) |>
    text_transform(
      locations = cells_body(
        columns = marktanteil,
        rows = marktanteil_diff > 0 & round(marktanteil_diff, 3) != 0
      ),
      fn = function(x) paste(x, up_arrow())
    ) |>
    text_transform(
      locations = cells_body(
        columns = marktanteil,
        rows = marktanteil_diff < 0 & round(marktanteil_diff, 3) != 0
      ),
      fn = function(x) paste(x, down_arrow())
    ) |>
    tab_source_note(
      df |>
        rename(
          "Herkunftsland" = Country,
          "Logiern\u00e4chte" = beobachtungsjahrLN,
          "Logiern\u00e4chte_Diff" = percent_changeLN,
          "Aufenthaltsdauer_in_Tagen" = dur_stay_beobachtung,
          "Aufenthaltsdauer_in_Tagen_Diff" = dur_stay_diff_abs,
          "Marktanteil" = marktanteil,
          "Marktanteil_Diff" = marktanteil_diff
        ) |>
        downloadthis::download_this(
          output_name = paste0(input$referenzregion, "_", input$beobachtungsjahr, "_", input$referenzjahr),
          output_extension = ".xlsx", # Excel output
          button_label = "Vollst\u00e4ndige Tabelle herunterladen",
          button_type = "primary"
        )
    ) |>
    opt_interactive(
      use_search = TRUE,
      use_resizers = TRUE,
      use_highlight = TRUE,
      use_compact_mode = TRUE,
      use_text_wrapping = TRUE,
      use_page_size_select = TRUE,
      page_size_values = c(5, 10, 25, 50),
      page_size_default = 5
    )
}

