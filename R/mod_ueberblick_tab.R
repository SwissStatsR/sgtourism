#' ueberblick_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for shiny.
#'
#' @importFrom shiny moduleServer eventReactive observeEvent renderUI reactive req NS tagList fluidRow span uiOutput br htmlOutput
#' @importFrom shinyWidgets pickerInput
#' @importFrom echarts4r echarts4rOutput
#' @importFrom bs4Dash box
#'
#' @keywords internal
#' @export
mod_ueberblick_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
        fluidRow(
          bs4Dash::column(
            width = 3,
            shinyWidgets::pickerInput(
              inputId = ns("beobachtungsregion"),
              label = uiOutput(ns("infoBeobachtungsregionUI")),
              choices = c("Schweiz", "Ostschweiz", "Kanton St.Gallen", "Destination Heidiland",
                          "Destination St.Gallen-Bodensee", "Destination Toggenburg",
                          "Destination Z\u00fcrichsee"),
              selected = "Kanton St.Gallen",
              options = list(style = "btn-outline-secondary")
            ),
            shinyWidgets::pickerInput(
              inputId = ns("referenzregion"),
              label = "Referenzregion:",
              choices = c("Schweiz", "Ostschweiz", "Kanton St.Gallen", "Destination Heidiland",
                          "Destination St.Gallen-Bodensee", "Destination Toggenburg",
                          "Destination Z\u00fcrichsee"),
              selected = "Schweiz",
              options = list(style = "btn-outline-secondary")
            )
          ),
          bs4Dash::column(
            width = 3,
            shinyWidgets::pickerInput(
              inputId = ns("beobachtungsjahr"),
              label = "Beobachtungsjahr:",
              choices = get_years(data = sgtourism::df_ueberblick)$year_choices_referenz,
              selected = get_years(data = sgtourism::df_ueberblick)$beobachtungsjahr_selected,
              options = list(style = "btn-outline-secondary")
            ),
            shinyWidgets::pickerInput(
              inputId = ns("referenzjahr"),
              label = "Referenzjahr:",
              choices = get_years(data = sgtourism::df_ueberblick)$year_choices_referenz,
              selected = get_years(data = sgtourism::df_ueberblick)$referenzjahr_selected,
              options = list(style = "btn-outline-secondary")
            )
          ),
          bs4Dash::column(
            width = 3,
            uiOutput(ns("monat_range_ui"))
          )
        ),
        br(),
        fluidRow(
          bs4Dash::box(
            id = ns("ankuenfte_box"),
            width = 3,
            title = uiOutput(ns("title_ankuenfte")),
            collapsible = FALSE,
            solidHeader = TRUE,
            htmlOutput(ns("box_ankuenfte"))
          ),
          bs4Dash::box(
            id = ns("logiernaechte_box"),
            width = 3,
            title = uiOutput(ns("title_logiernaechte")),
            collapsible = FALSE,
            solidHeader = TRUE,
            htmlOutput(ns("box_logiernaechte"))
          ),
          bs4Dash::box(
            id = ns("bed_occ_box"),
            width = 3,
            title = uiOutput(ns("title_bed_occ")),
            collapsible = FALSE,
            solidHeader = TRUE,
            htmlOutput(ns("box_bed_occ"))
          ),
          box(
            id = ns("dur_stay_box"),
            width = 3,
            title = uiOutput(ns("title_dur_stay")),
            collapsible = FALSE,
            solidHeader = TRUE,
            htmlOutput(ns("box_dur_stay"))
          )
        ),
        br(),
        fluidRow(
          bs4Dash::column(
            width = 6,
            box(
              id = ns("entwicklung_logiernaechte_box"),
              width = 12,
              collapsible = FALSE,
              solidHeader = TRUE,
              title = uiOutput(ns("title_entwicklung_logiernaechte")),
              echarts4r::echarts4rOutput(ns("entwicklung_logiernaechte"))
            )
          ),
          bs4Dash::column(
            width = 6,
            box(
              id = ns("veraenderung_logiernaechte_box"),
              width = 12,
              collapsible = FALSE,
              solidHeader = TRUE,
              title = uiOutput(ns("title_veraenderung_logiernaechte")),
              echarts4r::echarts4rOutput(ns("veraenderung_logiernaechte"))
            )
          )
        ),
        fluidRow(
          bs4Dash::column(
            width = 6,
            box(
              id = ns("gt_land_beobachtungsregion_box"),
              width = 12,
              collapsible = FALSE,
              title = uiOutput(ns("title_gt_beobachtungsregion")),
              gt::gt_output(ns("gt_beobachtungsregion"))
            )
          ),
          bs4Dash::column(
            width = 6,
            box(
              id = ns("gt_land_referenzregion_box"),
              width = 12,
              collapsible = FALSE,
              title = uiOutput(ns("title_gt_referenzregion")),
              gt::gt_output(ns("gt_referenzregion"))
            )
          )
        )
      )
}

#' ueberblick_tab Server Functions
#'
#' @param id Internal parameters for shiny.
#'
#' @keywords internal
#' @export
mod_ueberblick_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Create text label with question icon with popover content for beobachtung region dropdown input
    output$infoBeobachtungsregionUI <- renderUI({
      text_with_popover_icon(
        text = "Beobachtungsregion:",
        input_id = "infoBeobachtungsregion",
        popover_content = "Zur Abgrenzung der St.Galler Tourismusdestinationen und der Tourismusregion Ostschweiz siehe https://www.sg.ch/ueber-den-kanton-st-gallen/statistik/metainformationen/tourismusdestinationen.html"
      )
    })
    # Filter data based on regions and years selected in UI
    df_beobachtungsregion <- eventReactive(c(input$beobachtungsregion, input$beobachtungsjahr, input$referenzjahr), {
      filter_data(
        data = sgtourism::df_ueberblick,
        input_region = input$beobachtungsregion,
        input = input
      )
    })
    df_referenzregion <- eventReactive(c(input$referenzregion, input$beobachtungsjahr, input$referenzjahr), {
      filter_data(
        data = sgtourism::df_ueberblick,
        input_region = input$referenzregion,
        input = input
      )
    })
    # Create Beobachtungsmonate month dropdown input UI
    observeEvent(c(input$beobachtungsjahr), {
      output$monat_range_ui <- renderUI({
        create_months_ui(
          data = df_beobachtungsregion(),
          label_text = "Beobachtungsmonate:",
          input = input,
          ns = ns
        )
      })
    })
    # Reactive vector object extracted from input$monat single or multiple selection
    months_selected <- eventReactive(input$monat, {
      select_months(input = input)
    })
    # Create data for box tables
    df_box_beobachtung <- reactive({
      get_box_data(
        data = df_beobachtungsregion(),
        months_selected = months_selected(),
        input = input
      )
    })
    df_box_referenz <- reactive({
      get_box_data(
        data = df_referenzregion(),
        months_selected = months_selected(),
        input = input
      )
    })
    # Create box titles
    output$title_ankuenfte <- renderUI({
      create_box_title(
        input = input,
        input_id = "infoAnkuenfte",
        title = "Ank\u00fcnfte",
        popover_content = "Anzahl der G\u00e4ste (inkl. Kinder), die eine oder mehrere N\u00e4chte in einem Hotelleriebetrieb verbringen."
      )
    })
    output$title_logiernaechte <- renderUI({
      create_box_title(
        input = input,
        input_id = "infoLogiernaechte",
        title = "Logiern\u00e4chte",
        popover_content = "Anzahl der N\u00e4chte, die die G\u00e4ste (inkl. Kinder) in einem Hotelleriebetrieb verbringen."
      )
    })
    output$title_bed_occ <- renderUI({
      create_box_title(
        input = input,
        input_id = "infoBedOcc",
        title = "Bettenauslastung",
        popover_content = "Brutto-Bettenauslastung: Prozentsatz, zu dem die vorhandenen Betten der Hotelleriebetriebe mit G\u00e4sten belegt waren."
      )
    })
    output$title_dur_stay <- renderUI({
      create_box_title(
        input = input,
        input_id = "infoDurStay",
        title = "Aufenthaltsdauer",
        popover_content = "Anzahl der Logiern\u00e4chte dividiert durch die Anzahl der Ank\u00fcnfte."
      )
    })

    # Create boxes
    output$box_ankuenfte <- reactive({
      create_box(
        input = input,
        df_box_beobachtung = df_box_beobachtung(),
        df_box_referenz = df_box_referenz(),
        indicator = "Ankuenfte"
      )
    })
    output$box_logiernaechte <- reactive({
      create_box(
        input = input,
        df_box_beobachtung = df_box_beobachtung(),
        df_box_referenz = df_box_referenz(),
        indicator = "Logiernaechte"
      )
    })
    output$box_bed_occ <- reactive({
      create_box_pct(
        input = input,
        df_box_beobachtung = df_box_beobachtung(),
        df_box_referenz = df_box_referenz(),
        indicator = "bed_occ"
      )
    })
    output$box_dur_stay <- reactive({
      create_box_dur_stay(
        input = input,
        df_box_beobachtung = df_box_beobachtung(),
        df_box_referenz = df_box_referenz(),
        indicator = "dur_stay"
      )
    })

    # Create titles for plots
    output$title_entwicklung_logiernaechte <- renderUI({
      plot_line_years_title(input = input, title = "Logiern\u00e4chte")
    })
    output$title_veraenderung_logiernaechte <- renderUI({
      plot_bar_regions_title(input = input, title = "Ver\u00e4nderung Logiern\u00e4chte")
    })

    # Create entwicklung plot
    output$entwicklung_logiernaechte <- renderEcharts4r({
      plot_line_years(
        data = df_beobachtungsregion(),
        months_selected = months_selected(),
        variable = "Logiernaechte"
      )
    })
    # Create veraenderung plot
    output$veraenderung_logiernaechte <- renderEcharts4r({
      plot_bar_regions(
        input = input,
        df_beobachtungsregion = df_beobachtungsregion(),
        df_referenzregion = df_referenzregion(),
        months_selected = months_selected(),
        variable = "Ankuenfte"
      )
    })

    # Create titles for interactive gt tables
    output$title_gt_beobachtungsregion <- renderUI({
      create_gt_title(input = input, input_region = input$beobachtungsregion)
    })
    output$title_gt_referenzregion <- renderUI({
      create_gt_title(input = input, input_region = input$referenzregion)
    })

    # Create data for gt table beobachtungsregion
    gt_data_beobachtung <- reactive({
      create_gt_data_beobachtung(
        df_beobachtungsregion = df_beobachtungsregion(),
        df_box_beobachtung = df_box_beobachtung(),
        months_selected = months_selected()
      )
    })
    # Create gt table beobachtungsregion
    output$gt_beobachtungsregion <- gt::render_gt({
      create_gt_beobachtungsregion(
        data = gt_data_beobachtung(),
        input = input
      )
    })

    # Create data for gt table referenzregion
    gt_table_data_referenz <- reactive({
      create_gt_data_referenzregion(
        df_referenzregion = df_referenzregion(),
        df_box_referenz = df_box_referenz(),
        months_selected = months_selected(),
        gt_data_beobachtung = gt_data_beobachtung()
      )
    })
    # Create gt table referenzregion
    output$gt_referenzregion <- gt::render_gt({
      create_gt_referenzregion(
        gt_table_data_referenz = gt_table_data_referenz(),
        gt_data_beobachtung = gt_data_beobachtung(),
        input = input
      )
    })
  })
}

## To be copied in the UI
# mod_ueberblick_tab_ui("ueberblick_tab_1")

## To be copied in the server
# mod_ueberblick_tab_server("ueberblick_tab_1")
