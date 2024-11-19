#' perimeter_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for shiny.
#'
#' @importFrom shiny NS tagList
#' @importFrom leaflet renderLeaflet
#' @importFrom shinycssloaders withSpinner
#'
#' @keywords internal
#' @export
mod_perimeter_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      # bs4Dash::column(
      #   width = 12,
      #   shiny::uiOutput(ns("downloadButton"), style ='padding-bottom:5px; padding-right:25px; float:right;')
      # ),
      bs4Dash::column(
        width = 12,
        shinycssloaders::withSpinner(
          shiny::uiOutput(ns("perimeter"))
        )
      )
    )
  )
}

#' perimeter_tab Server Functions
#'
#' @param id Internal parameters for shiny.
#'
#' @importFrom leaflet leafletOutput leaflet addPolygons addTiles setMaxBounds addControl colorNumeric providerTileOptions highlightOptions leafletOptions
#' @importFrom leafsync sync
#' @importFrom htmlwidgets onRender prependContent
#' @importFrom downloadthis download_this
#' @import sf
#'
#' @keywords internal
#' @export
mod_perimeter_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # output$downloadButton <- shiny::renderUI({
    #   mtcars |>
    #     downloadthis::download_this(
    #       output_name = "data-test",
    #       output_extension = ".xlsx",
    #       button_label = "Tabelle herunterladen",
    #       button_type = "primary",
    #       has_icon = TRUE,
    #       icon = "fa fa-save"
    #     )
    # })

    output$perimeter <- shiny::renderUI({

      create_title <- function(title) {
        tags$div(
          HTML(title)
        )
      }
      get_pal_green <- colorNumeric(palette = c("#00953B", "#dedc85"), domain = dest$dest_nr)
      get_pal_blue <- colorNumeric(palette = c("#014A97", "#336DAB"), domain = dest_sub$dest_nr)
      get_pal_purple <- colorNumeric(palette = c("#722283", "#8E4E9B"), domain = dest_mun$HIST_NR)

      map_dest <- leaflet() |>
        addPolygons(
          data = dest,
          weight = 1,
          opacity = 1,
          color = "#00953B",
          fillColor = ~get_pal_green(dest_nr),
          fillOpacity = .5,
          label = ~paste0(dest_name),
          highlightOptions = highlightOptions(weight = 2, color = "#CC3333") # sgfarbpalette Mohnrot
        ) |>
        addTiles(attribution = "&copy swisstopo",
                 options = providerTileOptions(minZoom = 9, maxZoom = 10)) |>
        setMaxBounds(lng1 = bbox_perimeter[1], lat1 = bbox_perimeter[2],
                     lng2 = bbox_perimeter[3], lat2 = bbox_perimeter[4]) |>
        addControl(create_title("Destinationen"), position = "topleft") |>
        htmlwidgets::onRender("
                              function(el, x) {
                              var map = this;
                              map.zoomControl.setPosition('bottomleft');
                              }
                              ")

      map_destsub <- leaflet(options = leafletOptions(zoomControl = FALSE)) |>
        addPolygons(
          data = dest_sub,
          weight = .5,
          color = "#014A97",
          opacity = 1,
          fillColor = ~get_pal_green(dest_nr),
          fillOpacity = .5,
          label = ~paste0(aggregat_name),
          highlightOptions = highlightOptions(weight = 2, color = "#CC3333")
        ) |>
        setMaxBounds(lng1 = bbox_perimeter[1], lat1 = bbox_perimeter[2],
                     lng2 = bbox_perimeter[3], lat2 = bbox_perimeter[4]) |>
        addTiles(attribution = "&copy swisstopo",
                 urlTemplate = "", # empty background
                 options = providerTileOptions(minZoom = 9, maxZoom = 10)) |>
        addControl(create_title("Subregionen der Destinationen"), position = "topleft")

      map_muni <- leaflet(options = leafletOptions(zoomControl = FALSE)) |>
        addPolygons(
          data = dest_mun,
          weight = .3,
          color = "#722283",
          opacity = 1,
          fillColor = ~get_pal_green(dest_nr),
          fillOpacity = .5,
          label = ~paste0(NAME),
          highlightOptions = highlightOptions(weight = 2, color = "#CC3333")
        ) |>
        setMaxBounds(lng1 = bbox_perimeter[1], lat1 = bbox_perimeter[2],
                     lng2 = bbox_perimeter[3], lat2 = bbox_perimeter[4]) |>
        addTiles(attribution = "&copy swisstopo",
                 urlTemplate = "", # empty background
                 options = providerTileOptions(minZoom = 9, maxZoom = 10)) |>
        addControl(create_title("Gemeinden in den Destinationen"), position = "topleft")

      map_empty <- leaflet(
        options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)
      )

      leafsync::sync(map_dest, map_empty, map_destsub, map_muni, ncol = 2, sync = list(c(1, 3, 4))) |>
        htmlwidgets::prependContent(htmltools::tags$style(".leaflet-container { background-color:rgba(255,0,0,0.0); }"))
    })
  })
}

## To be copied in the UI
# mod_perimeter_tab_ui("perimeter_tab_1")

## To be copied in the server
# mod_perimeter_tab_server("perimeter_tab_1")
