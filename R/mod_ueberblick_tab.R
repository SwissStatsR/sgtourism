#' ueberblick_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny moduleServer eventReactive observeEvent renderUI reactive req NS tagList fluidRow span uiOutput br htmlOutput
#' @importFrom shinyWidgets pickerInput
#' @importFrom echarts4r echarts4rOutput
#' @importFrom bs4Dash box popover actionButton
mod_ueberblick_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
        fluidRow(
          bs4Dash::column(
            offset = 1,
            width = 3,
            shinyWidgets::pickerInput(
              inputId = ns("beobachtungsregion"),
              label = span(
                "Beobachtungsregion:",
                popover(
                  tag = bs4Dash::actionButton("infoBeobachtungsregion", label = NULL, icon = icon("question-circle"), class = "border-0 p-0"),
                  title = NULL,
                  content = "Zur Abgrenzung der St.Galler Tourismusdestinationen und der Tourismusregion Ostschweiz siehe https://www.sg.ch/ueber-den-kanton-st-gallen/statistik/metainformationen/tourismusdestinationen.html"
                ),
              ),
              choices = c("Schweiz", "Ostschweiz", "Kanton St.Gallen", "Heidiland", "St.Gallen-Bodensee", "Toggenburg", "Z\u00fcrichsee"),
              selected = "Kanton St.Gallen",
              options = list(style = "btn-outline-secondary")
            ),
            shinyWidgets::pickerInput(
              inputId = ns("referenzregion"),
              label = "Referenzregion:",
              choices = c("Schweiz", "Ostschweiz", "Kanton St.Gallen", "Heidiland", "St.Gallen-Bodensee", "Toggenburg", "Z\u00fcrichsee"),
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
              title = uiOutput(ns("title_gt_land_beobachtungsregion")),
              gt::gt_output(ns("gt_land_beobachtungsregion"))
            )
          ),
          bs4Dash::column(
            width = 6,
            box(
              id = ns("gt_land_referenzregion_box"),
              width = 12,
              collapsible = FALSE,
              title = uiOutput(ns("title_gt_land_referenzregion")),
              gt::gt_output(ns("gt_land_referenzregion"))
            )
          )
        )
      )
}

#' ueberblick_tab Server Functions
#'
#' @importFrom bs4Dash updateBox
#' @importFrom dplyr select filter mutate bind_rows distinct arrange pull group_by ungroup summarise left_join rename recode starts_with desc
#' @importFrom tidyr pivot_longer pivot_wider complete nesting
#' @importFrom shinyWidgets airDatepickerInput
#' @importFrom lubridate month
#' @importFrom echarts4r renderEcharts4r e_charts e_charts_ e_bar e_line_ e_color e_y_axis e_tooltip e_datazoom e_toolbox_feature e_title e_tooltip_item_formatter e_axis_formatter e_show_loading e_locale
#' @importFrom gt gt md fmt_number cols_merge cols_label text_transform cells_body opt_interactive sub_missing fmt_percent tab_source_note
#' @importFrom downloadthis download_this
#'
#' @noRd
mod_ueberblick_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Uberblick ---------------------------------------------------------------
    # Data
    df_beobachtungsregion <- eventReactive(c(input$beobachtungsregion, input$beobachtungsjahr, input$referenzjahr), {
      data <- if(input$beobachtungsregion == "Schweiz") {
        sgtourism::df_ueberblick |>
          filter(Jahr == input$beobachtungsjahr) |>
          mutate(Referenz = "beobachtungsjahr") |>
          bind_rows(
            sgtourism::df_ueberblick |>
              filter(Jahr == input$referenzjahr) |>
              mutate(Referenz = "referenzjahr")
          )
      } else if(input$beobachtungsregion == "Ostschweiz") {
        sgtourism::df_ueberblick |>
          filter(Kt %in% c(8, 14, 15, 16, 17, 20)) |>
          filter(Jahr == input$beobachtungsjahr) |>
          mutate(Referenz = "beobachtungsjahr") |>
          bind_rows(
            sgtourism::df_ueberblick |>
              filter(Kt %in% c(8, 14, 15, 16, 17, 20)) |>
              filter(Jahr == input$referenzjahr) |>
              mutate(Referenz = "referenzjahr")
          )
      } else if(input$beobachtungsregion == "Kanton St.Gallen") {
        sgtourism::df_ueberblick |>
          filter(Kt == 17) |>
          filter(Jahr == input$beobachtungsjahr) |>
          mutate(Referenz = "beobachtungsjahr") |>
          bind_rows(
            sgtourism::df_ueberblick |>
              filter(Kt == 17) |>
              filter(Jahr == input$referenzjahr) |>
              mutate(Referenz = "referenzjahr")
          )
      } else if(input$beobachtungsregion == "Heidiland") {
        sgtourism::df_ueberblick |>
          filter(Destination == 1) |>
          filter(Jahr == input$beobachtungsjahr) |>
          mutate(Referenz = "beobachtungsjahr") |>
          bind_rows(
            sgtourism::df_ueberblick |>
              filter(Destination == 1) |>
              filter(Jahr == input$referenzjahr) |>
              mutate(Referenz = "referenzjahr")
          )
      } else if(input$beobachtungsregion == "St.Gallen-Bodensee") {
        sgtourism::df_ueberblick |>
          filter(Destination == 2) |>
          filter(Jahr == input$beobachtungsjahr) |>
          mutate(Referenz = "beobachtungsjahr") |>
          bind_rows(
            sgtourism::df_ueberblick |>
              filter(Destination == 2) |>
              filter(Jahr == input$referenzjahr) |>
              mutate(Referenz = "referenzjahr")
          )
      } else if(input$beobachtungsregion == "Toggenburg") {
        sgtourism::df_ueberblick |>
          filter(Destination == 3) |>
          filter(Jahr == input$beobachtungsjahr) |>
          mutate(Referenz = "beobachtungsjahr") |>
          bind_rows(
            sgtourism::df_ueberblick |>
              filter(Destination == 3) |>
              filter(Jahr == input$referenzjahr) |>
              mutate(Referenz = "referenzjahr")
          )
      } else if(input$beobachtungsregion == "Z\u00fcrichsee") {
        sgtourism::df_ueberblick |>
          filter(Destination == 4) |>
          filter(Jahr == input$beobachtungsjahr) |>
          mutate(Referenz = "beobachtungsjahr") |>
          bind_rows(
            sgtourism::df_ueberblick |>
              filter(Destination == 4) |>
              filter(Jahr == input$referenzjahr) |>
              mutate(Referenz = "referenzjahr")
          )
      }
      return(data)
    })
    df_referenzregion <- eventReactive(c(input$referenzregion, input$beobachtungsjahr, input$referenzjahr), {
      data <- if(input$referenzregion == "Schweiz") {
        sgtourism::df_ueberblick |>
          filter(Jahr == input$beobachtungsjahr) |>
          mutate(Referenz = "beobachtungsjahr") |>
          bind_rows(
            sgtourism::df_ueberblick |>
              filter(Jahr == input$referenzjahr) |>
              mutate(Referenz = "referenzjahr")
          )
      }
      else if(input$referenzregion == "Ostschweiz") {
        sgtourism::df_ueberblick |>
          filter(Kt %in% c(8, 14, 15, 16, 17, 20)) |>
          filter(Jahr == input$beobachtungsjahr) |>
          mutate(Referenz = "beobachtungsjahr") |>
          bind_rows(
            sgtourism::df_ueberblick |>
              filter(Kt %in% c(8, 14, 15, 16, 17, 20)) |>
              filter(Jahr == input$referenzjahr) |>
              mutate(Referenz = "referenzjahr")
          )
      }
      else if(input$referenzregion == "Kanton St.Gallen") {
        sgtourism::df_ueberblick |>
          filter(Kt == 17) |>
          filter(Jahr == input$beobachtungsjahr) |>
          mutate(Referenz = "beobachtungsjahr") |>
          bind_rows(
            sgtourism::df_ueberblick |>
              filter(Kt == 17) |>
              filter(Jahr == input$referenzjahr) |>
              mutate(Referenz = "referenzjahr")
          )
      }
      else if(input$referenzregion == "Heidiland") {
        sgtourism::df_ueberblick |>
          filter(Destination == 1) |>
          filter(Jahr == input$beobachtungsjahr) |>
          mutate(Referenz = "beobachtungsjahr") |>
          bind_rows(
            sgtourism::df_ueberblick |>
              filter(Destination == 1) |>
              filter(Jahr == input$referenzjahr) |>
              mutate(Referenz = "referenzjahr")
          )
      }
      else if(input$referenzregion == "St.Gallen-Bodensee") {
        sgtourism::df_ueberblick |>
          filter(Destination == 2) |>
          filter(Jahr == input$beobachtungsjahr) |>
          mutate(Referenz = "beobachtungsjahr") |>
          bind_rows(
            sgtourism::df_ueberblick |>
              filter(Destination == 2) |>
              filter(Jahr == input$referenzjahr) |>
              mutate(Referenz = "referenzjahr")
          )
      }
      else if(input$referenzregion == "Toggenburg") {
        sgtourism::df_ueberblick |>
          filter(Destination == 3) |>
          filter(Jahr == input$beobachtungsjahr) |>
          mutate(Referenz = "beobachtungsjahr") |>
          bind_rows(
            sgtourism::df_ueberblick |>
              filter(Destination == 3) |>
              filter(Jahr == input$referenzjahr) |>
              mutate(Referenz = "referenzjahr")
          )
      }
      else if(input$referenzregion == "Z\u00fcrichsee") {
        sgtourism::df_ueberblick |>
          filter(Destination == 4) |>
          filter(Jahr == input$beobachtungsjahr) |>
          mutate(Referenz = "beobachtungsjahr") |>
          bind_rows(
            sgtourism::df_ueberblick |>
              filter(Destination == 4) |>
              filter(Jahr == input$referenzjahr) |>
              mutate(Referenz = "referenzjahr")
          )
      }
      return(data)
    })

    # UI Monat, updated only when Beobachtungsjahr input is changed
    observeEvent(c(input$beobachtungsjahr), {

      output$monat_range_ui <- renderUI({

        beobachtungsjahr_unique_month <- df_beobachtungsregion() |>
          filter(Referenz == "beobachtungsjahr") |>
          distinct(Jahr, Monat) |>
          arrange(Monat) |>
          pull(Monat)

        startmonat_selected <- as.Date(paste(input$beobachtungsjahr, beobachtungsjahr_unique_month[1], "1", sep = "-"))
        endmonat_selected <- as.Date(paste(input$beobachtungsjahr, sort(beobachtungsjahr_unique_month, decreasing = TRUE)[1], "1", sep = "-"))

        airDatepickerInput(
          inputId = ns("monat"),
          language = "de",
          label = "Beobachtungsmonate:",
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
      })
    })

    # reactive vector object for input$monat, could be one or multiple months selected
    months_selected <- eventReactive(input$monat, {
      if (length(input$monat) == 2) {
        c(lubridate::month(input$monat[1]) : lubridate::month(input$monat[2]))
      } else if (length(input$monat) == 1) {
        c(lubridate::month(input$monat))
      }
    })

    df_box_beobachtung <- reactive({

      df_box_beobachtungsregion <- df_beobachtungsregion() |>
        filter(Monat %in% months_selected()) |>
        group_by(Referenz) |>
        summarise(Ankuenfte = sum(Ankuenfte, na.rm = TRUE),
                  Logiernaechte = sum(Logiernaechte, na.rm = TRUE),
                  bed_occ = sum(Logiernaechte) / sum (Bettenmonat),
                  dur_stay = sum(Logiernaechte) / sum (Ankuenfte)
        ) |>
        ungroup()

      # get absolute and percentage differences
      df_box_beobachtungsregion |>
        arrange(Referenz) |>
        pivot_longer(
          cols = c(Ankuenfte, Logiernaechte, bed_occ, dur_stay),
          names_to = "Indicator",
          values_to = "Abs_diff") |>
        pivot_wider(
          names_from = Referenz,
          values_from = Abs_diff
        ) |>
        mutate(diff_abs = beobachtungsjahr - referenzjahr,
               percent_change = (beobachtungsjahr - referenzjahr)/ referenzjahr * 100 #,
               #mean = (beobachtungsjahr + referenzjahr) / 2,
               #diff_perc = abs(diff_abs) / mean * 100
        )
    })
    df_box_referenz <- reactive({
      req(input$referenzregion, months_selected())

      df_box_referenzregion <- df_referenzregion() |>
        filter(Monat %in% months_selected()) |>
        group_by(Referenz) |>
        summarise(Ankuenfte = sum(Ankuenfte, na.rm = TRUE),
                  Logiernaechte = sum(Logiernaechte, na.rm = TRUE),
                  bed_occ = sum(Logiernaechte) / sum (Bettenmonat),
                  dur_stay = sum(Logiernaechte) / sum (Ankuenfte)
        ) |>
        ungroup()

      # get absolute and percentage differences
      df_box_referenzregion |>
        arrange(Referenz) |>
        pivot_longer(
          cols = c(Ankuenfte, Logiernaechte, bed_occ, dur_stay),
          names_to = "Indicator",
          values_to = "Abs_diff") |>
        pivot_wider(
          names_from = Referenz,
          values_from = Abs_diff
        ) |>
        mutate(diff_abs = beobachtungsjahr - referenzjahr,
               percent_change = (beobachtungsjahr - referenzjahr)/ referenzjahr * 100 #,
               #mean = (beobachtungsjahr + referenzjahr) / 2,
               #diff_perc = abs(diff_abs) / mean * 100
        )
    })
    # box titles
    output$title_ankuenfte <- renderUI({
      tags$span(
        tags$b(paste0("Ank\u00fcnfte ", input$beobachtungsjahr)),
        popover(
          tag = bs4Dash::actionButton(
            inputId = "infoAnkunfte",
            label = NULL,
            icon = icon("question-circle"),
            class = "border-0 p-0"
          ),
          title = NULL,
          content = "Anzahl der G\u00e4ste (inkl. Kinder), die eine oder mehrere N\u00e4chte in einem Hotelleriebetrieb verbringen."
        )
      )
    })
    output$title_logiernaechte <- renderUI({
      tags$span(
        tags$b(paste0("Logiern\u00e4chte ", input$beobachtungsjahr)),
        popover(
          tag = bs4Dash::actionButton(
            inputId = "infoLogiernaechte",
            label = NULL,
            icon = icon("question-circle"),
            class = "border-0 p-0"
          ),
          title = NULL,
          content = "Anzahl der N\u00e4chte, die die G\u00e4ste (inkl. Kinder) in einem Hotelleriebetrieb verbringen."
        )
      )
    })
    output$title_bed_occ <- renderUI({
      tags$span(tags$b(paste0("Bettenauslastung ", input$beobachtungsjahr)),
                popover(
                  tag = bs4Dash::actionButton(
                    inputId = "infoBettenauslastung",
                    label = NULL,
                    icon = icon("question-circle"),
                    class = "border-0 p-0"
                  ),
                  title = NULL,
                  content = "Brutto-Bettenauslastung: Prozentsatz, zu dem die vorhandenen Betten der Hotelleriebetriebe mit G\u00e4sten belegt waren."
                )
      )
    })
    output$title_dur_stay <- renderUI({
      tags$span(tags$b(paste0("Aufenthaltsdauer  ", input$beobachtungsjahr)),
                popover(
                  tag = bs4Dash::actionButton("infoAufenthaltsdauer", label = NULL, icon = icon("question-circle"), class = "border-0 p-0"),
                  title = NULL,
                  content = "Anzahl der Logiern\u00e4chte dividiert durch die Anzahl der Ank\u00fcnfte."
                )
      )
    })
    output$title_entwicklung_logiernaechte <- renderUI({
      # tags$b(paste("Entwicklung Logiern\u00e4chte ", input$beobachtungsregion))
      tags$b(paste("Entwicklung Logiern\u00e4chte"))
    })
    output$title_veraenderung_logiernaechte <- renderUI({
      tags$b(paste("Ver\u00e4nderung Logiern\u00e4chte ", input$referenzjahr, " - ", input$beobachtungsjahr))
    })
    output$title_gt_land_beobachtungsregion <- renderUI({
      tags$div(tags$b(paste0(input$beobachtungsregion, ", ", input$beobachtungsjahr)), tags$small(paste0(" (Ver\u00e4nderung gg\u00fc. ", input$referenzjahr, ")")))
    })
    output$title_gt_land_referenzregion <- renderUI({
      tags$div(tags$b(paste0(input$referenzregion, ", ", input$beobachtungsjahr)), tags$small(paste0(" (Ver\u00e4nderung gg\u00fc. ", input$referenzjahr, ")")))
    })
    # Boxes
    output$box_ankuenfte <- reactive({

      df_box_beobachtung_ankuenfte <- df_box_beobachtung() |>
        filter(Indicator == "Ankuenfte")
      df_box_referenz_ankuenfte <- df_box_referenz() |>
        filter(Indicator == "Ankuenfte")

      #color_beobachtung_jahr <- get_number_color(df_box_beobachtung_ankuenfte$beobachtungsjahr)
      color_beobachtung_diff_abs <- get_number_color(df_box_beobachtung_ankuenfte$diff_abs)
      color_beobachtung_percent_change <- get_number_color(df_box_beobachtung_ankuenfte$percent_change)
      #color_referenz_jahr <- get_number_color(df_box_referenz_ankuenfte$beobachtungsjahr)
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

      paste0("<span style='font-size: 1.1em;'>",
             input$beobachtungsregion,
             "</span>",
             "<br>",
             "<b style='font-size: 1.2em;'>",
             prettyNum(df_box_beobachtung_ankuenfte$beobachtungsjahr, big.mark = "'"),
             "</b>",
             "<br>",
             #"absolut: ",
             "<b style='color:", color_beobachtung_diff_abs,"';>",
             add_plus_if_needed_beobachtung_diff_abs,
             prettyNum(df_box_beobachtung_ankuenfte$diff_abs, big.mark = "'"),
             " ", icon_beobachtung_diff_abs,
             "</b>",
             "<br>",
             #"prozentual: ",
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
             #"absolut: ",
             "<b style='color:", color_referenz_diff_abs,"';>",
             add_plus_if_needed_referenz_diff_abs,
             prettyNum(df_box_referenz_ankuenfte$diff_abs, big.mark = "'"),
             " ", icon_referenz_diff_abs,
             "</b>",
             "<br>",
             #"prozentual: ",
             "<b style='color:", color_referenz_percent_change,"';>",
             add_plus_if_needed_referenz_percent_change,
             formatC(df_box_referenz_ankuenfte$percent_change, digits = 1, format = "f"), "%",
             " ", icon_referenz_percent_change,
             "</b>",
             "<br>",
             "<br>",
             "<i>",
             "Ver\u00e4nderung gg\u00fc: ", input$referenzjahr,
             "</i>"
      )
    })
    output$box_logiernaechte <- reactive({
      req(months_selected())

      df_box_beobachtung_logiernaechte <- df_box_beobachtung() |>
        filter(Indicator == "Logiernaechte")
      df_box_referenz_logiernaechte <- df_box_referenz() |>
        filter(Indicator == "Logiernaechte")

      #color_beobachtung_jahr <- get_number_color(df_box_beobachtung_logiernaechte$beobachtungsjahr)
      color_beobachtung_diff_abs <- get_number_color(df_box_beobachtung_logiernaechte$diff_abs)
      color_beobachtung_percent_change <- get_number_color(df_box_beobachtung_logiernaechte$percent_change)
      #color_referenz_jahr <- get_number_color(df_box_referenz_logiernaechte$beobachtungsjahr)
      color_referenz_diff_abs <- get_number_color(df_box_referenz_logiernaechte$diff_abs)
      color_referenz_percent_change <- get_number_color(df_box_referenz_logiernaechte$percent_change)

      icon_beobachtung_diff_abs <- get_number_icon(df_box_beobachtung_logiernaechte$diff_abs)
      icon_beobachtung_percent_change <- get_number_icon(df_box_beobachtung_logiernaechte$percent_change)
      icon_referenz_diff_abs <- get_number_icon(df_box_referenz_logiernaechte$diff_abs)
      icon_referenz_percent_change <- get_number_icon(df_box_referenz_logiernaechte$percent_change)

      add_plus_if_needed_beobachtung_diff_abs <- add_plus_if_needed(df_box_beobachtung_logiernaechte$diff_abs)
      add_plus_if_needed_beobachtung_percent_change<- add_plus_if_needed(df_box_beobachtung_logiernaechte$percent_change)
      add_plus_if_needed_referenz_diff_abs <- add_plus_if_needed(df_box_referenz_logiernaechte$diff_abs)
      add_plus_if_needed_referenz_percent_change <- add_plus_if_needed(df_box_referenz_logiernaechte$percent_change)

      paste0("<span style='font-size: 1.1em;'>",
             input$beobachtungsregion,
             "</span>",
             "<br>",
             "<b style='font-size: 1.2em;'>",
             prettyNum(df_box_beobachtung_logiernaechte$beobachtungsjahr, big.mark = "'"),
             "</b>",
             "<br>",
             #"absolut: ",
             "<b style='color:", color_beobachtung_diff_abs, "';>",
             add_plus_if_needed_beobachtung_diff_abs,
             prettyNum(df_box_beobachtung_logiernaechte$diff_abs, big.mark = "'"),
             " ", icon_beobachtung_diff_abs,
             "</b>",
             "<br>",
             #"prozentual: ",
             "<b style='color:", color_beobachtung_percent_change, "';>",
             add_plus_if_needed_beobachtung_percent_change,
             formatC(df_box_beobachtung_logiernaechte$percent_change, digits = 1, format = "f"), "%",
             " ", icon_beobachtung_percent_change,
             "</b>",
             "<br>",
             "<br>",
             "<span style='font-size: 1.1em;'>", input$referenzregion, "</span>",
             "<br>",
             "<b style='font-size: 1.2em;'>",
             prettyNum(df_box_referenz_logiernaechte$beobachtungsjahr, big.mark = "'"),
             "</b>",
             "<br>",
             #"absolut: ",
             "<b style='color:", color_referenz_diff_abs, "';>",
             add_plus_if_needed_referenz_diff_abs,
             prettyNum(df_box_referenz_logiernaechte$diff_abs, big.mark = "'"),
             " ", icon_referenz_diff_abs,
             "</b>",
             "<br>",
             #"prozentual: ",
             "<b style='color:", color_referenz_percent_change, "';>",
             add_plus_if_needed_referenz_percent_change,
             formatC(df_box_referenz_logiernaechte$percent_change, digits = 1, format = "f"), "%",
             " ", icon_referenz_percent_change,
             "</b>",
             "<br>",
             "<br>",
             "<i>",
             "Ver\u00e4nderung gg\u00fc: ", input$referenzjahr,
             "</i>"
      )
    })
    output$box_bed_occ <- reactive({
      req(input$beobachtungsregion, months_selected())

      df_box_beobachtung_bed_occ <- df_box_beobachtung() |>
        filter(Indicator == "bed_occ")
      df_box_referenz_bed_occ <- df_box_referenz() |>
        filter(Indicator == "bed_occ")

      #color_beobachtung_jahr <- get_number_color(df_box_beobachtung_bed_occ$beobachtungsjahr)
      color_beobachtung_diff_abs <- get_number_color(df_box_beobachtung_bed_occ$beobachtungsjahr - df_box_beobachtung_bed_occ$referenzjahr)
      #color_referenz_jahr <- get_number_color(df_box_referenz_bed_occ$beobachtungsjahr)
      color_referenz_diff_abs <- get_number_color(df_box_referenz_bed_occ$beobachtungsjahr - df_box_referenz_bed_occ$referenzjahr)

      icon_beobachtung_diff_abs <- get_number_icon(df_box_beobachtung_bed_occ$beobachtungsjahr - df_box_beobachtung_bed_occ$referenzjahr)
      icon_referenz_diff_abs <- get_number_icon(df_box_referenz_bed_occ$beobachtungsjahr - df_box_referenz_bed_occ$referenzjahr)

      add_plus_if_needed_beobachtung_diff_abs <- add_plus_if_needed(df_box_beobachtung_bed_occ$beobachtungsjahr - df_box_beobachtung_bed_occ$referenzjahr)
      add_plus_if_needed_referenz_diff_abs <- add_plus_if_needed(df_box_referenz_bed_occ$beobachtungsjahr - df_box_referenz_bed_occ$referenzjahr)

      paste0("<span style='font-size: 1em;'>",
             input$beobachtungsregion,
             "</span>",
             "<br>",
             "<b style='font-size: 1.2em;'>",
             formatC(df_box_beobachtung_bed_occ$beobachtungsjahr * 100, digits = 1, format = "f"), "%",
             "</b>",
             "<br>",
             #"absolut: ",
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
             #"absolut: ",
             "<b style='color:", color_referenz_diff_abs, "';>",
             add_plus_if_needed_referenz_diff_abs,
             formatC((df_box_referenz_bed_occ$beobachtungsjahr - df_box_referenz_bed_occ$referenzjahr) * 100, digits = 1, format = "f"), "%-Pkt",
             " ", icon_referenz_diff_abs,
             "</b>",
             "<br>",
             "<br>",
             "<br>",
             "<i>",
             "Ver\u00e4nderung gg\u00fc: ", input$referenzjahr,
             "</i>"
      )
    })
    output$box_dur_stay <- reactive({
      req(input$beobachtungsregion, months_selected())

      df_box_beobachtung_dur_stay <- df_box_beobachtung() |>
        filter(Indicator == "dur_stay")
      df_box_referenz_dur_stay <- df_box_referenz() |>
        filter(Indicator == "dur_stay")

      #color_beobachtung_jahr <- get_number_color(df_box_beobachtung_dur_stay$beobachtungsjahr)
      color_beobachtung_diff_abs <- get_number_color(df_box_beobachtung_dur_stay$diff_abs)
      color_beobachtung_percent_change <- get_number_color(df_box_beobachtung_dur_stay$percent_change)
      #color_referenz_jahr <- get_number_color(df_box_referenz_dur_stay$beobachtungsjahr)
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

      paste0("<span style='font-size: 1.1em;'>",
             input$beobachtungsregion,
             "</span>",
             "<br>",
             "<b style='font-size: 1.2em;'>",
             formatC(df_box_beobachtung_dur_stay$beobachtungsjahr, digits = 2, format = "f"),
             ifelse(df_box_beobachtung_dur_stay$beobachtungsjahr > 1, " Tage", " Tag"),
             "</b>",
             "<br>",
             #"absolut: ",
             "<b style='color:", color_beobachtung_diff_abs, "';>",
             add_plus_if_needed_beobachtung_diff_abs,
             formatC(df_box_beobachtung_dur_stay$diff_abs, digits = 2, format = "f"),
             " ", icon_beobachtung_diff_abs,
             "</b>",
             "<br>",
             #"prozentual: ",
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
             #"absolut: ",
             "<b style='color:", color_referenz_diff_abs, "';>",
             add_plus_if_needed_referenz_diff_abs,
             formatC(df_box_referenz_dur_stay$diff_abs, digits = 2, format = "f"),
             " ", icon_referenz_diff_abs,
             "</b>",
             "<br>",
             #"prozentual: ",
             "<b style='color:", color_referenz_percent_change, "';>",
             add_plus_if_needed_referenz_percent_change,
             formatC(df_box_referenz_dur_stay$percent_change, digits = 1, format = "f"), "%",
             " ", icon_referenz_percent_change,
             "</b>",
             "<br>",
             "<br>",
             "<i>",
             "Ver\u00e4nderung gg\u00fc: ", input$referenzjahr,
             "</i>"
      )
    })
    output$entwicklung_logiernaechte <- renderEcharts4r({
      req(months_selected(), df_beobachtungsregion())

      df <- df_beobachtungsregion() |>
        filter(Monat %in% months_selected()) |>
        mutate(Monat = month_abb_de()[Monat]) |> # month names
        mutate(Jahr = as.factor(Jahr)) |>
        group_by(Jahr, Monat) |>
        summarise(Logiernaechte_total = sum(Logiernaechte, na.rm = TRUE)) |>
        ungroup() |>
        complete(Monat = month_abb_de(), nesting(Jahr), fill = list(Logiernaechte_total = NA)) |>
        pivot_wider(names_from = Jahr, values_from = Logiernaechte_total) |>
        mutate(Monat = factor(Monat, levels = month_abb_de())) |>
        arrange(Monat)

      if (length(names(df)) == 3) {
        df |>
          e_charts_(names(df)[1]) |>
          e_line_(names(df)[2]) |>
          e_line_(names(df)[3]) |>
          e_color(color = color_palette(name = "intensiv")) |>
          e_tooltip(trigger = "axis") |>
          e_datazoom() |>
          e_toolbox_feature(feature = c("saveAsImage", "dataView")) |>
          e_title(text = input$beobachtungsregion) |>
          e_show_loading(color = "#7AB800") |>
          e_locale("DE")
      } else if (length(names(df)) == 2) {
        df |>
          e_charts_(names(df)[1]) |>
          e_line_(names(df)[2]) |>
          e_color(color = color_palette(name = "intensiv")) |>
          e_tooltip(trigger = "axis") |>
          e_datazoom() |>
          e_toolbox_feature(feature = c("saveAsImage", "dataView")) |>
          e_title(text = input$beobachtungsregion) |>
          e_show_loading(color = "#7AB800") |>
          e_locale("DE")
      }

    })
    output$veraenderung_logiernaechte <- renderEcharts4r({
      req(months_selected(),  df_beobachtungsregion())

      df_beobachtung <- df_beobachtungsregion() |>
        mutate(region_type = "beobachtungsregion") |>
        mutate(Monat = as.factor(Monat)) |>
        group_by(region_type, Referenz, Monat) |>
        summarise(Logiernaechte_total = sum(Logiernaechte, na.rm = TRUE)) |>
        ungroup() |>
        pivot_wider(names_from = Referenz, values_from = Logiernaechte_total) |>
        mutate(percent_change = (beobachtungsjahr - referenzjahr)/ referenzjahr)

      df_referenz <- df_referenzregion() |>
        mutate(region_type = "referenzregion") |>
        mutate(Monat = as.factor(Monat)) |>
        group_by(region_type, Referenz, Monat) |>
        summarise(Logiernaechte_total = sum(Logiernaechte, na.rm = TRUE)) |>
        ungroup() |>
        pivot_wider(names_from = Referenz, values_from = Logiernaechte_total) |>
        mutate(percent_change = (beobachtungsjahr - referenzjahr)/ referenzjahr)

      df_beobachtung |>
        bind_rows(df_referenz) |>
        filter(Monat %in% months_selected()) |>
        mutate(Monat = month_abb_de()[Monat]) |> # month names
        complete(Monat = month_abb_de(), nesting(region_type), fill = list(percent_change = NA)) |>
        mutate(Monat = factor(Monat, levels = month_abb_de())) |>
        arrange(Monat) |>
        mutate(region_type = recode(region_type, "beobachtungsregion" = input$beobachtungsregion, "referenzregion" = input$referenzregion)) |>
        group_by(region_type) |>
        e_charts(Monat) |>
        e_bar(percent_change) |>
        e_color(color = color_palette(name = "rein")) |>
        e_tooltip(
          formatter = e_tooltip_item_formatter("percent", digits = 1)
        ) |>
        e_y_axis(
          formatter = e_axis_formatter("percent", digits = 0)
        ) |>
        e_datazoom() |>
        e_toolbox_feature(feature = c("saveAsImage", "dataView")) |>
        e_show_loading(color = "#7AB800") |>
        e_locale("DE")
    })

    gt_table_data_beobachtung <- reactive({

      df_eda_ln <- df_beobachtungsregion() |>
        filter(Monat %in% months_selected()) |>
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

      df_eda_ak <- df_beobachtungsregion() |>
        filter(Monat %in% months_selected()) |>
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
      val_beobachtungsjahr_logiernaechte <- df_box_beobachtung() |>
        filter(Indicator == "Logiernaechte") |>
        pull(beobachtungsjahr)

      val_referenzjahr_logiernaechte <- df_box_beobachtung() |>
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
    })

    output$gt_land_beobachtungsregion <- gt::render_gt({

      df <- gt_table_data_beobachtung() |>
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
        # # "+" not added when using cols_merge()
        # text_transform(
        #   locations = cells_body(
        #     columns = dur_stay_diff_abs,
        #     rows = dur_stay_diff_abs > 0
        #   ),
        #   fn = function(x) paste0("+", x)
        # ) |>
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
          Country = with_tooltip("Herkunftsland", "Lorem Ipsum"),
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
    })

    gt_table_data_referenz <- reactive({
      df_eda_ln <- df_referenzregion() |>
        filter(Monat %in% months_selected()) |>
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

      df_eda_ak <- df_referenzregion() |>
        filter(Monat %in% months_selected()) |>
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
      val_beobachtungsjahr_logiernaechte <- df_box_referenz() |>
        filter(Indicator == "Logiernaechte") |>
        pull(beobachtungsjahr)

      val_referenzjahr_logiernaechte <- df_box_referenz() |>
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

      # reorder country based on order of gt_table_data_beobachtung
      country_order <- gt_table_data_beobachtung() |> arrange(desc(marktanteil)) |> pull(Country)
      df_reordered <- df_cleaned[match(country_order, df_cleaned$Country),]
      df_reordered
    })

    output$gt_land_referenzregion <- gt::render_gt({

      #observe({print(setdiff(gt_table_data_beobachtung() |> distinct(Country) |> pull(Country), gt_table_data_referenz() |> distinct(Country) |> pull(Country)))})

      df <- gt_table_data_referenz()
      country_order <- gt_table_data_beobachtung() |> arrange(desc(marktanteil)) |> pull(Country)

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
        # # "+" not added when using cols_merge()
        # text_transform(
        #   locations = cells_body(
        #     columns = dur_stay_diff_abs,
        #     rows = dur_stay_diff_abs > 0
        #   ),
        #   fn = function(x) paste0("+", x)
        # ) |>
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
    })
  })
}

## To be copied in the UI
# mod_ueberblick_tab_ui("ueberblick_tab_1")

## To be copied in the server
# mod_ueberblick_tab_server("ueberblick_tab_1")
