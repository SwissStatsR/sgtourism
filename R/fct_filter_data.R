#' Filter raw data based on some UI inputs
#'
#' This function create a filtered data based on the UI inputs referenzregion or
#' beobachtungsregion, beobachtungsjahr and referenzjahr. The function creates
#' a `Referenz` variable to be used in other functions in the server.
#'
#' @param data dataset `sgtourism::df_ueberblick`.
#' @param input_region input region, either input$beobachtungsregion or
#' input$referenzregion.
#' @param input Ueberblick input beobachtungsregion, beobachtungsjahr and
#' referenzjahr.
#'
#' @importFrom dplyr filter mutate bind_rows
#'
#' @return a filtered data.frame
#'
#' @noRd
filter_data <- function(data, input_region, input) {
  aggregat_lookup <- sgtourism::lookup_aggregat$aggregat_cd[sgtourism::lookup_aggregat$aggregat_tx == input_region]
  df_filtered <- sgtourism::df_ueberblick |>
    filter(Aggregat == aggregat_lookup) |>
    filter(Jahr == input$beobachtungsjahr) |>
    mutate(Referenz = "beobachtungsjahr") |>
    bind_rows(
      sgtourism::df_ueberblick |>
        filter(Aggregat == aggregat_lookup) |>
        filter(Jahr == input$referenzjahr) |>
        mutate(Referenz = "referenzjahr")
    )
  return(df_filtered)
}
