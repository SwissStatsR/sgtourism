#' functions for drawing different kind of plots

#' Create a two-lines echarts plot (if beobachtungsjahr <> referenzjahr)
#' @param df data fram to plot
#' @param color_plot color palette 'name' argument from `color_palette()`
#'
#' @return line plot for two years
#'
#'
#' @noRd
plot_line_multiple <-function(df, color_plot){
  df |>
    e_charts_(names(df)[1]) |>
    e_line_(names(df)[2]) |>
    e_line_(names(df)[3]) |>
    e_color(color = color_palette(name = color_plot)) |>
    e_tooltip(trigger = "axis") |>
    e_datazoom(toolbox = FALSE)|>
    e_toolbox_feature(feature = c("saveAsImage", "dataView")) |>
    e_show_loading(color = "#7AB800") |>
    e_locale("CH")
}


#' Create a echart plot with a single line plot (if beobachtungsjahr = referenzjahr)
#'
#' @param df data fram to plot
#' @param color_plot color palette 'name' argument from `color_palette()`
#'
#' @return line plot for single year
#'
#' @noRd
plot_line_single <-function(df, color_plot){
  df |>
    e_charts_(names(df)[1]) |>
    e_line_(names(df)[2]) |>
    e_color(color = color_palette(name = color_plot)) |>
    e_tooltip(trigger = "axis") |>
    e_datazoom(toolbox = FALSE) |>
    e_toolbox_feature(feature = c("saveAsImage", "dataView")) |>
    e_show_loading(color = "#7AB800") |>
    e_locale("CH")
}


#' Draw col plot
#'
#' @param df_beobachtungsregion reactive df_beobachtungsregion
#' @param df_referenzregion reactive df_referenzregion
#' @param variables variables to plot
#' @param input input
#' @param months_selected reactive input selected months
#'
#' @return col plot
#'
#' @noRd
plot_bar_change <- function(df_beobachtungsregion, df_referenzregion, variables, input, months_selected){

  month_abb_de <- month_abb_de()

  df_beobachtungsregion_filtered <- df_beobachtungsregion |>
    mutate(region_type = "beobachtungsregion") |>
    mutate(Monat = as.factor(Monat)) |>
    group_by(region_type, Referenz, Monat) |>
    summarise(Total = sum({{ variables }}, na.rm = TRUE)) |>
    ungroup() |>
    pivot_wider(names_from = Referenz, values_from = Total) |>
    mutate(percent_change = (beobachtungsjahr - referenzjahr)/ referenzjahr)

  df_referenzregion_filtered <- df_referenzregion |>
    mutate(region_type = "referenzregion") |>
    mutate(Monat = as.factor(Monat)) |>
    group_by(region_type, Referenz, Monat) |>
    summarise(Total = sum({{ variables }}, na.rm = TRUE)) |>
    ungroup() |>
    pivot_wider(names_from = Referenz, values_from = Total) |>
    mutate(percent_change = (beobachtungsjahr - referenzjahr)/ referenzjahr)

  df_beobachtungsregion_filtered |>
    bind_rows(df_referenzregion_filtered) |>
    filter(Monat %in% months_selected) |>
    mutate(Monat = month_abb_de[Monat]) |> # month names
    complete(Monat = month_abb_de, nesting(region_type), fill = list(percent_change = NA)) |>
    mutate(Monat = factor(Monat, levels = month_abb_de)) |>
    arrange(Monat) |>
    mutate(region_type = recode(region_type, "beobachtungsregion" = input$beobachtungsregion, "referenzregion" = input$referenzregion)) |>
    group_by(region_type) |>
    e_charts(Monat) |>
    e_bar(percent_change) |>
    e_color(color = color_palette(name = "pal_sg_2")) |>
    e_tooltip(
      formatter = e_tooltip_item_formatter("percent", digits = 1)
    ) |>
    e_y_axis(
      formatter = e_axis_formatter("percent", digits = 0)
    ) |>
    e_toolbox_feature(feature = c("saveAsImage", "dataView")) |>
    e_show_loading(color = "#7AB800") |>
    e_locale("CH")
}

#' Create title for plot years
#'
#' @param title title
#' @param input input
#'
#' @return an html object
#'
#' @noRd
plot_line_years_title <- function(title, input) {
  tags$b(paste0(title, " ", input$beobachtungsregion))
}

#' Create echarts4r plot to compare year values of a given variable
#'
#' Create plots based on data and conditionality
#'
#' @param data data
#' @param months_selected vector from `months_selected()`
#' @param variable variable to compare the year values
#'
#' @return an echarts4r object
#'
#' @noRd
plot_line_years <- function(data, months_selected, variable) {

  var <- as.name(variable)

  df <- data |>
    filter(Monat %in% months_selected) |>
    mutate(Monat = month_abb_de()[Monat]) |> # month names
    mutate(Jahr = as.factor(Jahr)) |>
    group_by(Jahr, Monat) |>
    summarise(Total = sum(!!var, na.rm = TRUE)) |>
    ungroup() |>
    complete(Monat = month_abb_de(), nesting(Jahr), fill = list(Total = NA)) |>
    pivot_wider(names_from = Jahr, values_from = Total) |>
    mutate(Monat = factor(Monat, levels = month_abb_de())) |>
    arrange(Monat)

  if (length(names(df)) == 3) {
    plot_line_multiple(df = df, color_plot = "pal_sg_1")
  } else if (length(names(df)) == 2) {
    plot_line_single(df = df, color_plot = "pal_sg_1")
  }
}

#' Create title for plot regions
#'
#' @param title title
#' @param input input
#'
#' @return an html object
#'
#' @noRd
plot_bar_regions_title <- function(title, input) {
  tags$b(paste0(title, " ", input$referenzjahr, " - ", input$beobachtungsjahr))
}

#' Create echarts4r plot to compare regions
#'
#' @param input input
#' @param df_beobachtungsregion reactive df_beobachtungsregion()
#' @param df_referenzregion reactive df_referenzregion()
#' @param months_selected reactive input months_selected()
#' @param variable variable
#'
#' @return echarts4r object
#'
#' @noRd
plot_bar_regions <- function(input, df_beobachtungsregion, df_referenzregion, months_selected, variable){

  var <- as.name(variable)

  df_beobachtung <- df_beobachtungsregion |>
    mutate(region_type = "beobachtungsregion") |>
    mutate(Monat = as.factor(Monat)) |>
    group_by(region_type, Referenz, Monat) |>
    summarise(Total = sum(!!var, na.rm = TRUE)) |>
    ungroup() |>
    pivot_wider(names_from = Referenz, values_from = Total) |>
    mutate(percent_change = (beobachtungsjahr - referenzjahr)/ referenzjahr)

  df_referenz <- df_referenzregion |>
    mutate(region_type = "referenzregion") |>
    mutate(Monat = as.factor(Monat)) |>
    group_by(region_type, Referenz, Monat) |>
    summarise(Total = sum(!!var, na.rm = TRUE)) |>
    ungroup() |>
    pivot_wider(names_from = Referenz, values_from = Total) |>
    mutate(percent_change = (beobachtungsjahr - referenzjahr)/ referenzjahr)

  month_abb_de <- month_abb_de()

  df_beobachtung |>
    bind_rows(df_referenz) |>
    filter(Monat %in% months_selected) |>
    mutate(Monat = month_abb_de[Monat]) |> # month names
    complete(Monat = month_abb_de, nesting(region_type), fill = list(percent_change = NA)) |>
    mutate(Monat = factor(Monat, levels = month_abb_de)) |>
    arrange(Monat) |>
    mutate(region_type = recode(region_type, "beobachtungsregion" = input$beobachtungsregion, "referenzregion" = input$referenzregion)) |>
    group_by(region_type) |>
    e_charts(Monat) |>
    e_bar(percent_change) |>
    e_color(color = color_palette(name = "pal_sg_2")) |>
    e_tooltip(
      formatter = e_tooltip_item_formatter("percent", digits = 1)
    ) |>
    e_y_axis(
      formatter = e_axis_formatter("percent", digits = 0)
    ) |>
    e_toolbox_feature(feature = c("saveAsImage", "dataView")) |>
    e_show_loading(color = "#7AB800") |>
    e_locale("CH") |>
    e_datazoom(toolbox = FALSE)
}

