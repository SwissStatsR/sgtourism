#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_ueberblick_tab_server("ueberblick_tab_1")
  mod_details_tab_server("details_tab_1")
  mod_perimeter_tab_server("perimeter_tab_1")
}
