#' mod_details_tab_ui UI Function
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
mod_details_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::column(
        # offset = 1,
        width = 3,
        shinyWidgets::pickerInput(
          inputId = ns("region"),
          label = "Analyseebene:",
          # choices = c("Kanton, Wahlkreise, Gemeinden", unique(sgtourism::df_ueberblick$Destination_tx)),
          choices = c("Kanton, Wahlkreise, Gemeinden", "Destination Heidiland", "Destination St.Gallen-Bodensee", "Destination Toggenburg", "Destination Z\u00fcrichsee"),
          selected = "Kanton, Wahlkreise, Gemeinden",
          options = list(style = "btn-outline-secondary")
        )
      ),
      bs4Dash::column(
        width = 3,
        uiOutput(ns("detailsIndicatorsUI"))
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
        uiOutput(ns("monat_range_ui")
        )
      )
    ),
    br(),
    bs4Dash::tabsetPanel(
      type = "tabs",
      tab("Angebot",
          fluidRow(
            bs4Dash::box(
              id = ns("betriebe_box"),
              width = 3,
              title = uiOutput(ns("title_betriebe")),
              collapsible = FALSE,
              solidHeader = TRUE,
              htmlOutput(ns("box_betriebe"))
            ),
            bs4Dash::box(
              id = ns("betten_box"),
              width = 3,
              title = uiOutput(ns("title_betten")),
              collapsible = FALSE,
              solidHeader = TRUE,
              htmlOutput(ns("box_betten"))
            ),
            bs4Dash::box(
              id = ns("zimmer_box"),
              width = 3,
              title = uiOutput(ns("title_zimmer")),
              collapsible = FALSE,
              solidHeader = TRUE,
              htmlOutput(ns("box_zimmer"))
            ),
            bs4Dash::box(
              id = ns("hot_size_box"),
              width = 3,
              title = uiOutput(ns("title_hot_size")),
              collapsible = FALSE,
              solidHeader = TRUE,
              htmlOutput(ns("box_hot_size"))
            ),
          ),
          br(),
          fluidRow(
            bs4Dash::column(
              width = 4,
              box(
                id = ns("entwicklung_betriebe_box"),
                width = 12,
                collapsible = FALSE,
                solidHeader = TRUE,
                title = uiOutput(ns("title_entwicklung_betriebe")),
                echarts4r::echarts4rOutput(ns("entwicklung_betriebe"))
              )
            ),
            bs4Dash::column(
              width = 4,
              box(
                id = ns("entwicklung_betten_box"),
                width = 12,
                collapsible = FALSE,
                solidHeader = TRUE,
                title = uiOutput(ns("title_entwicklung_betten")),
                echarts4r::echarts4rOutput(ns("entwicklung_betten"))
              )
            ),
            # )
            # ,
            # br(),
            # fluidRow(
            bs4Dash::column(
              width = 4,
              box(
                id = ns("entwicklung_groesse_box"),
                width = 12,
                collapsible = FALSE,
                solidHeader = TRUE,
                title = uiOutput(ns("title_entwicklung_groesse")),
                echarts4r::echarts4rOutput(ns("entwicklung_groesse"))
              )
            )
          )
      ),
      tab("Nachfrage",
          fluidRow(
            bs4Dash::column(
              width = 3,
              shinyWidgets::pickerInput(
                inputId = ns("markt"),
                label = "Herkunftsland / Markt:",
                choices = "Total", # sgtourism::meta_countries$Country2,
                selected = "Total",
                options = list(style = "btn-outline-secondary")
              )
            )
          ),
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
              id = ns("dur_stay_box"),
              width = 3,
              title = uiOutput(ns("title_dur_stay")),
              collapsible = FALSE,
              solidHeader = TRUE,
              htmlOutput(ns("box_dur_stay"))
            ),
            bs4Dash::box(
              id = ns("zimmernaechte_box"),
              width = 3,
              title = uiOutput(ns("title_zimmernaechte")),
              collapsible = FALSE,
              solidHeader = TRUE,
              htmlOutput(ns("box_zimmernaechte"))
            ),
          ),
          br(),
          fluidRow(
            bs4Dash::column(
              width = 4,
              box(
                id = ns("veraenderung_ankuenfte_box"),
                width = 12,
                collapsible = FALSE,
                solidHeader = TRUE,
                title = uiOutput(ns("title_veraenderung_ankuenfte")),
                echarts4r::echarts4rOutput(ns("veraenderung_ankuenfte")
                )
              )
            ),
            bs4Dash::column(
              width = 4,
              box(
                id = ns("veraenderung_logiernaechte_box"),
                width = 12,
                collapsible = FALSE,
                solidHeader = TRUE,
                title = uiOutput(ns("title_veraenderung_logiernaechte")),
                echarts4r::echarts4rOutput(ns("veraenderung_logiernaechte"))
              )
            ),
            bs4Dash::column(
              width = 4,
              box(
                id = ns("veraenderung_zimmernaechte_box"),
                width = 12,
                collapsible = FALSE,
                solidHeader = TRUE,
                title = uiOutput(ns("title_veraenderung_zimmernaechte")),
                echarts4r::echarts4rOutput(ns("veraenderung_zimmernaechte"))
              )
            )
          ),
          br(),
          fluidRow(
            bs4Dash::column(
              width = 6,
              box(
                id = ns("entwicklung_ankuenfte_box"),
                width = 12,
                collapsible = FALSE,
                solidHeader = TRUE,
                title = uiOutput(ns("title_entwicklung_ankuenfte")),
                echarts4r::echarts4rOutput(ns("entwicklung_ankuenfte"))
              )
            ),
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
            )
          ),
          br(),
          fluidRow(
            bs4Dash::column(
              width = 6,
              box(
                id = ns("entwicklung_dur_stay_box"),
                width = 12,
                collapsible = FALSE,
                solidHeader = TRUE,
                title = uiOutput(ns("title_entwicklung_dur_stay")),
                echarts4r::echarts4rOutput(ns("entwicklung_dur_stay"))
              )
            ),
            bs4Dash::column(
              width = 6,
              box(
                id = ns("entwicklung_zimmernaechte_box"),
                width = 12,
                collapsible = FALSE,
                solidHeader = TRUE,
                title = uiOutput(ns("title_entwicklung_zimmernaechte")),
                echarts4r::echarts4rOutput(ns("entwicklung_zimmernaechte"))
              )
            )
          )
      ),
      tab("Auslastung",
          fluidRow(
            bs4Dash::box(
              id = ns("bed_occ_box"),
              width = 3,
              title = uiOutput(ns("title_bed_occ")),
              collapsible = FALSE,
              solidHeader = TRUE,
              htmlOutput(ns("box_bed_occ"))
            ),
            bs4Dash::box(
              id = ns("bed_net_occ_box"),
              width = 3,
              title = uiOutput(ns("title_bed_net_occ")),
              collapsible = FALSE,
              solidHeader = TRUE,
              htmlOutput(ns("box_bed_net_occ"))
            ),
            bs4Dash::box(
              id = ns("room_occ_box"),
              width = 3,
              title = uiOutput(ns("title_room_occ")),
              collapsible = FALSE,
              solidHeader = TRUE,
              htmlOutput(ns("box_room_occ"))
            ),
            bs4Dash::box(
              id = ns("room_net_occ_box"),
              width = 3,
              title = uiOutput(ns("title_room_net_occ")),
              collapsible = FALSE,
              solidHeader = TRUE,
              htmlOutput(ns("box_room_net_occ"))
            ),
          ),
          br(),
          fluidRow(
            bs4Dash::column(
              width = 4,
              box(
                id = ns("entw_kurz_bed_occ_box"),
                width = 12,
                collapsible = FALSE,
                solidHeader = TRUE,
                title = uiOutput(ns("title_entw_kurz_bed_occ")),
                echarts4r::echarts4rOutput(ns("entw_kurz_bed_occ")
                )
              )
            ),
            bs4Dash::column(
              width = 4,
              box(
                id = ns("entw_kurz_bed_net_occ_box"),
                width = 12,
                collapsible = FALSE,
                solidHeader = TRUE,
                title = uiOutput(ns("title_entw_kurz_bed_net_occ")),
                echarts4r::echarts4rOutput(ns("entw_kurz_bed_net_occ"))
              )
            ),
            bs4Dash::column(
              width = 4,
              box(
                id = ns("entw_kurz_room_occ_box"),
                width = 12,
                collapsible = FALSE,
                solidHeader = TRUE,
                title = uiOutput(ns("title_entw_kurz_room_occ")),
                echarts4r::echarts4rOutput(ns("entw_kurz_room_occ"))
              )
            )
          ),
          br(),
          fluidRow(
            bs4Dash::column(
              width = 4,
              box(
                id = ns("entwicklung_bed_occ_box"),
                width = 12,
                collapsible = FALSE,
                solidHeader = TRUE,
                title = uiOutput(ns("title_entwicklung_bed_occ")),
                echarts4r::echarts4rOutput(ns("entwicklung_bed_occ")
                )
              )
            ),
            bs4Dash::column(
              width = 4,
              box(
                id = ns("entwicklung_bed_net_occ_box"),
                width = 12,
                collapsible = FALSE,
                solidHeader = TRUE,
                title = uiOutput(ns("title_entwicklung_bed_net_occ")),
                echarts4r::echarts4rOutput(ns("entwicklung_bed_net_occ"))
              )
            ),
            bs4Dash::column(
              width = 4,
              box(
                id = ns("entwicklung_room_occ_box"),
                width = 12,
                collapsible = FALSE,
                solidHeader = TRUE,
                title = uiOutput(ns("title_entwicklung_room_occ")),
                echarts4r::echarts4rOutput(ns("entwicklung_room_occ"))
              )
            )
          )
      )
    )
  )
}

#' mod_details_tab_server Server Functions
#'
#' @importFrom bs4Dash updateBox
#' @importFrom dplyr select filter mutate bind_rows distinct arrange pull group_by ungroup summarise left_join rename recode starts_with desc
#' @importFrom tidyr pivot_longer pivot_wider complete nesting expand_grid
#' @importFrom shinyWidgets airDatepickerInput
#' @importFrom lubridate month
#' @importFrom echarts4r renderEcharts4r e_charts e_charts_ e_bar e_line_ e_color e_y_axis e_tooltip e_datazoom e_toolbox_feature e_title e_tooltip_item_formatter e_axis_formatter e_show_loading e_locale e_dims
#' @importFrom gt gt md fmt_number cols_merge cols_label text_transform cells_body opt_interactive sub_missing fmt_percent tab_source_note
#' @importFrom downloadthis download_this
#'
#' @noRd
mod_details_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # detailsIndicatorsUI
    output$detailsIndicatorsUI <- renderUI({
      choices_beobachtungsregion <- if(input$region == "Destination Heidiland") {
        list('Destination Heidiland' = c("Destination Heidiland"),
             'Destination Heidiland' = c("Subregion Bad Ragaz", "Subregion Pizol (ohne Bad Ragaz)",
                                               "Subregion Flumserberg", "Subregion Walensee", "Subregion Walensee",
                                               "Subregion B\u00fcndner Herrschaft /F\u00fcnf D\u00f6rfer", "Destination Heidiland Teil SG"))
      } else if(input$region == "Destination St.Gallen-Bodensee") {
        list('Destination St.Gallen-Bodensee' = c("Destination St.Gallen-Bodensee"),
             'Teilgebiete der Destination' = c("Subregion St.Gallen", "Subregion Bodensee", "Subregion Rheintal",
                                               "Subregion F\u00fcrstenland"))
      } else if(input$region == "Destination Toggenburg") {
        list('Destination Toggenburg' = c("Destination Toggenburg"),
             'Teilgebiete der Destination' = c("Subregion Wildhaus-Alt St. Johann", "Subregion Nesslau / Ebnat-Kappel",
                                               "Subregion Neckertal", "Subregion Mittleres / Unteres Toggenburg"))
      } else if(input$region == "Destination Z\u00fcrichsee") {
        list('Destination Z\u00fcrichsee' = c("Destination Z\u00fcrichsee"),
             'Teilgebiete der Destination' = c("Subregion Rapperswil-Jona", "Subregion Linthebene",
                                               "Subregion Z\u00fcri-Oberland", "Subregion Z\u00fcrichsee",
                                               "Subregion Ausserschwyz", "Destination Z\u00fcrichsee Teil SG"))
      } else if(input$region == "Kanton, Wahlkreise, Gemeinden") {
        list('Kanton St.Gallen' = c("Kanton St.Gallen"),
             'Wahlkreise' = c("Wahlkreis St.Gallen", "Wahlkreis Rorschach", "Wahlkreis Rheintal", "Wahlkreis Werdenberg",
                              "Wahlkreis Sarganserland", "Wahlkreis See-Gaster", "Wahlkreis Toggenburg", "Wahlkreis Wil" ),
             'Gemeinden' = c("Amden", "Bad Ragaz", "Buchs (SG)", "Flums", "Gossau (SG)", "Quarten", "Rapperswil-Jona",
                             "St.Gallen", "Uzwil", "Vilters-Wangs", "Walenstadt", "Wil (SG)", "Wildhaus-Alt St. Johann"))
      }
      selected_beobachtungsregion <- if(input$region == "Destination Heidiland") {
        c("Subregion Bad Ragaz")
      } else if(input$region == "Destination St.Gallen-Bodensee") {
        c("Subregion St.Gallen")
      } else if(input$region == "Destination Toggenburg") {
        c("Subregion Wildhaus-Alt St. Johann")
      } else if(input$region == "Destination Z\u00fcrichsee") {
        c("Subregion Rapperswil-Jona")
      } else if(input$region == "Kanton, Wahlkreise, Gemeinden") {
        c("Wahlkreis St.Gallen")
      }
      choices_referenzregion <- if(input$region == "Destination Heidiland") {
        list('Destination Heidiland' = c("Destination Heidiland"),
             'Teilgebiete der Destination' = c("Subregion Bad Ragaz", "Subregion Pizol (ohne Bad Ragaz)",
                                               "Subregion Flumserberg", "Subregion Walensee", "Subregion Walensee",
                                               "Subregion B\u00fcndner Herrschaft / F\u00fcnf D\u00f6rfer", "Destination Heidiland Teil SG"),
             '\u00dcbergeordnete Ebenen' = c("Kanton St.Gallen", "Ostschweiz", "Schweiz"),
             'Andere Destinationen' = c("Destination St.Gallen-Bodensee", "Destination Toggenburg", "Destination Z\u00fcrichsee"))
        } else if(input$region == "Destination St.Gallen-Bodensee") {
          list('Destination St.Gallen-Bodensee' = c("Destination St.Gallen-Bodensee"),
               'Teilgebiete der Destination' = c("Subregion St.Gallen", "Subregion Bodensee",
                                                 "Subregion Rheintal", "Subregion F\u00fcrstenland"),
               '\u00dcbergeordnete Ebenen' = c("Kanton St.Gallen", "Ostschweiz", "Schweiz"),
               'Andere Destinationen' = c("Destination Heidiland", "Destination Toggenburg", "Destination Z\u00fcrichsee"))
        } else if(input$region == "Destination Toggenburg") {
          list('Destination Toggenburg' = c("Destination Toggenburg"),
               'Teilgebiete der Destination' = c("Subregion Wildhaus-Alt St. Johann", "Subregion Nesslau / Ebnat-Kappel",
                                                 "Subregion Neckertal", "Subregion Mittleres / Unteres Toggenburg"),
               '\u00dcbergeordnete Ebenen' = c("Kanton St.Gallen", "Ostschweiz", "Schweiz"),
               'Andere Destinationen' = c("Destination Heidiland", "Destination St.Gallen-Bodensee", "Destination Z\u00fcrichsee"))
        } else if(input$region == "Destination Z\u00fcrichsee") {
          list('Destination Z\u00fcrichsee' = c("Destination Z\u00fcrichsee"),
               'Teilgebiete der Destination' = c("Subregion Rapperswil-Jona", "Subregion Linthebene",
                                               "Subregion Z\u00fcri-Oberland", "Subregion Z\u00fcrichsee",
                                               "Subregion Ausserschwyz", "Destination Z\u00fcrichsee Teil SG"),
               '\u00dcbergeordnete Ebenen' = c("Kanton St.Gallen", "Ostschweiz", "Schweiz"),
               'Andere Destinationen' = c("Destination Heidiland", "Destination Heidiland", "Destination St.Gallen-Bodensee"))
        } else if(input$region == "Kanton, Wahlkreise, Gemeinden") {
          list('\u00dcbergeordnete Ebenen' = c("Kanton St.Gallen", "Ostschweiz", "Schweiz"),
               'Wahlkreise' = c("Wahlkreis St.Gallen", "Wahlkreis Rorschach", "Wahlkreis Rheintal", "Wahlkreis Werdenberg",
                              "Wahlkreis Sarganserland", "Wahlkreis See-Gaster", "Wahlkreis Toggenburg", "Wahlkreis Wil" ),
               'Gemeinden' = c("Amden", "Bad Ragaz", "Buchs (SG)", "Flums", "Gossau (SG)", "Quarten", "Rapperswil-Jona",
                             "St.Gallen", "Uzwil", "Vilters-Wangs", "Walenstadt", "Wil (SG)", "Wildhaus-Alt St. Johann"))
      }
      selected_referenzregion <- if(input$region == "Destination Heidiland") {
        c("Destination Heidiland")
        } else if(input$region == "Destination St.Gallen-Bodensee") {
          c("Destination St.Gallen-Bodensee")
        } else if(input$region == "Destination Toggenburg") {
          c("Destination Toggenburg")
        } else if(input$region == "Destination Z\u00fcrichsee") {
          c("Destination Z\u00fcrichsee")
        } else if(input$region == "Kanton, Wahlkreise, Gemeinden") {
        c("Kanton St.Gallen")
      }
      tagList(
        shinyWidgets::pickerInput(
          inputId = ns("beobachtungsregion"),
          label = "Beobachtungsregion:",
          choices = choices_beobachtungsregion,
          selected = selected_beobachtungsregion,
          options = list(style = "btn-outline-secondary")
        ),
        shinyWidgets::pickerInput(
          inputId = ns("referenzregion"),
          label = "Referenzregion:",
          choices = choices_referenzregion,
          selected = choices_referenzregion[1],
          options = list(style = "btn-outline-secondary")
        )
      )
    })

    # Filter data based on regions and years selected in UI
    df_beobachtungsregion <- eventReactive(c(input$beobachtungsregion, input$beobachtungsjahr, input$referenzjahr), {
      req(input$beobachtungsregion)
      filter_data(
        data = sgtourism::df_ueberblick,
        input_region = input$beobachtungsregion,
        input = input
      )
    })
    df_referenzregion <- eventReactive(c(input$referenzregion, input$beobachtungsjahr, input$referenzjahr), {
      req(input$beobachtungsregion)
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
        input = input,
        df_beobachtungsregion = df_beobachtungsregion(),
        df_referenzregion = df_referenzregion()
      )
    })
    df_box_referenz <- reactive({
      get_box_data(
        data = df_referenzregion(),
        months_selected = months_selected(),
        input = input
      )
    })

    # Supply tab --------------------------------------------------------------

    # Create box titles
    output$title_betriebe <- renderUI({
      create_box_title(
        title = "Betriebe",
        input = input,
        input_id = "infoBetriebe",
        popover_content = "Durchschnittliche Anzahl der erfassten Betriebe in der
          Hotellerie w\u00e4hrend der betreffenden Periode. Zur Hotellerie z\u00e4hlen
          Hotels (Hotels, Pensionen, Gasth\u00e4user, Motels usw.) und Kurbetriebe
          (Kurh\u00e4user, alpine Heilst\u00e4tten, Volksheilb\u00e4der, usw.)."
      )
    })
    output$title_betten <- renderUI({
      create_box_title(
        title = "Betten",
        input = input,
        input_id = "infoBetten",
        popover_content = "Durchschnittliche Anzahl der Betten in allen erfassten Betrieben w\u00e4hrend der betreffenden Periode."
      )
    })
    output$title_zimmer <- renderUI({
      create_box_title(
        title = "Zimmer",
        input = input,
        input_id = "infoZimmer",
        popover_content = "Durchschnittliche Anzahl der Zimmer in allen erfassten Betrieben w\u00e4hrend der betreffenden Periode."
      )
    })
    output$title_hot_size <- renderUI({
      create_box_title(
        title = "Betten je Betrieb",
        input = input,
        input_id = "infoBetriebsgroesse",
        popover_content = "Durchschnittliche Anzahl der Betten je Betrieb w\u00e4hrend der betreffenden Periode."
      )
    })
    output$title_entwicklung_betriebe <- renderUI({
      tags$b(paste("Anzahl Betriebe ", min(as.numeric(input$referenzjahr), as.numeric(input$beobachtungsjahr), max(as.numeric(input$referenzjahr), as.numeric(input$beobachtungsjahr)) - 10)," - ", max(input$referenzjahr, input$beobachtungsjahr)))
    })
    output$title_entwicklung_betten <- renderUI({
      tags$b(paste("Anzahl Betten ", min(as.numeric(input$referenzjahr), as.numeric(input$beobachtungsjahr), max(as.numeric(input$referenzjahr), as.numeric(input$beobachtungsjahr)) - 10)," - ", max(input$referenzjahr, input$beobachtungsjahr)))
    })
    output$title_entwicklung_groesse <- renderUI({
      tags$b(paste("Betriebsgroesse (Betten je Betrieb) ", min(as.numeric(input$referenzjahr), as.numeric(input$beobachtungsjahr), max(as.numeric(input$referenzjahr), as.numeric(input$beobachtungsjahr)) - 10)," - ", max(input$referenzjahr, input$beobachtungsjahr)))
    })
    output$box_betriebe <- eventReactive(c(input$beobachtungsregion, input$referenzregion, input$referenzjahr, df_box_beobachtung(), df_box_referenz()), {
      create_box(
        input = input,
        df_box_beobachtung = df_box_beobachtung(),
        df_box_referenz = df_box_referenz(),
        indicator = "Betriebe"
      )
    })
    output$box_betten <- eventReactive(c(input$beobachtungsregion, input$referenzregion, input$referenzjahr, df_box_beobachtung(), df_box_referenz()), {
      create_box(
        input = input,
        df_box_beobachtung = df_box_beobachtung(),
        df_box_referenz = df_box_referenz(),
        indicator = "Betten"
      )
    })
    output$box_zimmer <- eventReactive(c(input$beobachtungsregion, input$referenzregion, input$referenzjahr, df_box_beobachtung(), df_box_referenz()), {
      create_box(
        input = input,
        df_box_beobachtung = df_box_beobachtung(),
        df_box_referenz = df_box_referenz(),
        indicator = "Zimmer"
      )
    })
    output$box_hot_size <- eventReactive(c(input$beobachtungsregion, input$referenzregion, input$referenzjahr, df_box_beobachtung(), df_box_referenz()), {
      create_box(
        input = input,
        df_box_beobachtung = df_box_beobachtung(),
        df_box_referenz = df_box_referenz(),
        indicator = "hot_size"
      )
    })

    output$entwicklung_betriebe <- renderEcharts4r({
      req(df_beobachtungsregion(), df_referenzregion())

      df_beobachtung <- expand_grid(Jahr = c(min(min(df_beobachtungsregion()$Jahr), max(df_beobachtungsregion()$Jahr - 10)):max(df_beobachtungsregion()$Jahr)), Monat = 1:12, Aggregat = unique(df_beobachtungsregion()$Aggregat)) |>
        # show at least 10 years
        left_join(sgtourism::df_ueberblick) |>
        filter(!is.na(Betriebe))|>
        mutate(region_type = "beobachtungsregion") |>
        group_by(region_type, Jahr) |>
        summarise(Betriebe_total = round(sum(Betriebe, na.rm = TRUE) / max(Monat), 0)) |>
        ungroup()

      df_referenz <- expand_grid(Jahr = c(min(min(df_referenzregion()$Jahr), max(df_referenzregion()$Jahr - 10)):max(df_referenzregion()$Jahr)), Monat = 1:12, Aggregat = unique(df_referenzregion()$Aggregat)) |>
        left_join(sgtourism::df_ueberblick) |>
        filter(!is.na(Betriebe))|>
        mutate(region_type = "referenzregion") |>
        group_by(region_type, Jahr) |>
        summarise(Betriebe_total = round(sum(Betriebe, na.rm = TRUE) / max(Monat), 0)) |>
        ungroup()

      df <- df_beobachtung |>
        bind_rows(df_referenz) |>
        mutate(region_type = recode(region_type, "beobachtungsregion" = input$beobachtungsregion, "referenzregion" = input$referenzregion)) |>
        mutate(Jahr = as.factor(Jahr))|>
        pivot_wider(names_from = region_type, values_from = Betriebe_total)

      if (length(names(df)) == 3) {
        plot_line_multiple(df, "rein")
      } else if (length(names(df)) == 2) {
        plot_line_single(df, "rein")
      }

    })
    output$entwicklung_betten <- renderEcharts4r({
      req(df_beobachtungsregion(), df_referenzregion())

      df_beobachtung <- expand_grid(Jahr = c(min(min(df_beobachtungsregion()$Jahr), max(df_beobachtungsregion()$Jahr - 10)):max(df_beobachtungsregion()$Jahr)), Monat = 1:12, Aggregat = unique(df_beobachtungsregion()$Aggregat)) |>
        left_join(sgtourism::df_ueberblick) |>
        filter(!is.na(Betten))|>
        mutate(region_type = "beobachtungsregion") |>
        group_by(region_type, Jahr) |>
        summarise(Betten_total = round(sum(Betten, na.rm = TRUE) / max(Monat), 0)) |>
        ungroup()

      df_referenz <- expand_grid(Jahr = c(min(min(df_referenzregion()$Jahr), max(df_referenzregion()$Jahr - 10)):max(df_referenzregion()$Jahr)), Monat = 1:12, Aggregat = unique(df_referenzregion()$Aggregat)) |>
        left_join(sgtourism::df_ueberblick) |>
        filter(!is.na(Betten))|>
        mutate(region_type = "referenzregion") |>
        group_by(region_type, Jahr) |>
        summarise(Betten_total = round(sum(Betten, na.rm = TRUE) / max(Monat), 0)) |>
        ungroup()

      df <- df_beobachtung |>
        bind_rows(df_referenz) |>
        mutate(region_type = recode(region_type, "beobachtungsregion" = input$beobachtungsregion, "referenzregion" = input$referenzregion)) |>
        mutate(Jahr = as.factor(Jahr))|>
        pivot_wider(names_from = region_type, values_from = Betten_total)

      if (length(names(df)) == 3) {
        plot_line_multiple(df, "rein")
      } else if (length(names(df)) == 2) {
        plot_line_single(df, "rein")
      }
    })

    output$entwicklung_groesse <- renderEcharts4r({
      req(df_beobachtungsregion(), df_referenzregion())

      df_beobachtung <- expand_grid(Jahr = c(min(min(df_beobachtungsregion()$Jahr), max(df_beobachtungsregion()$Jahr - 10)):max(df_beobachtungsregion()$Jahr)), Monat = 1:12, Aggregat = unique(df_beobachtungsregion()$Aggregat)) |>
        left_join(sgtourism::df_ueberblick) |>
        filter(!is.na(Betten))|>
        mutate(region_type = "beobachtungsregion") |>
        group_by(region_type, Jahr) |>
        summarise(Betriebsgroesse = round((sum(Betten, na.rm = TRUE) / max(Monat)) / (sum(Betriebe, na.rm = TRUE) / max(Monat)), 0)) |>
        ungroup()

      df_referenz <- expand_grid(Jahr = c(min(min(df_referenzregion()$Jahr), max(df_referenzregion()$Jahr - 10)):max(df_referenzregion()$Jahr)), Monat = 1:12, Aggregat = unique(df_referenzregion()$Aggregat)) |>
        left_join(sgtourism::df_ueberblick) |>
        filter(!is.na(Betten))|>
        mutate(region_type = "referenzregion") |>
        group_by(region_type, Jahr) |>
        summarise(Betriebsgroesse = round((sum(Betten, na.rm = TRUE) / max(Monat)) / (sum(Betriebe, na.rm = TRUE) / max(Monat)), 0)) |>
        ungroup()

      df <- df_beobachtung |>
        bind_rows(df_referenz) |>
        mutate(region_type = recode(region_type, "beobachtungsregion" = input$beobachtungsregion, "referenzregion" = input$referenzregion)) |>
        mutate(Jahr = as.factor(Jahr))|>
        pivot_wider(names_from = region_type, values_from = Betriebsgroesse)

      if (length(names(df)) == 3) {
        plot_line_multiple(df, "rein")
      } else if (length(names(df)) == 2) {
        plot_line_single(df, "rein")
      }
    })

    # Demand tab --------------------------------------------------------------

    output$title_ankuenfte <- renderUI({
      create_box_title(
        title = "Ank\u00fcnfte",
        input = input,
        input_id = "infoAnkuenfteSGBS",
        popover_content = "Anzahl der G\u00e4ste (inkl. Kinder) aus einem Herkunftsland, die eine oder mehrere N\u00e4chte in einem Hotelleriebetrieb verbringen. Das Herkunftsland ist das Land des st\u00e4ndigen Wohnsitzes des Gastes. Dieses Land muss nicht der Nationalit\u00e4t des Gastes entsprechen."
      )
    })
    output$title_logiernaechte <- renderUI({
      create_box_title(
        title = "Logiern\u00e4chte",
        input = input,
        input_id = "infoLogiernaechteSGBS",
        popover_content = "Anzahl der N\u00e4chte, die die G\u00e4ste (inkl. Kinder) aus einem Herkunftsland in einem Hotelleriebetrieb verbringen. Das Herkunftsland ist das Land des st\u00e4ndigen Wohnsitzes des Gastes. Dieses Land muss nicht der Nationalit\u00e4t des Gastes entsprechen."
      )
    })
    output$title_dur_stay <- renderUI({
      create_box_title(
        title = "Aufenthaltsdauer",
        input = input,
        input_id = "infoDur_staySGBS",
        popover_content = "Durchschnittliche Anzahl der Zimmer in allen erfassten Betrieben w\u00e4hrend der betreffenden Periode."
      )
    })
    output$title_zimmernaechte <- renderUI({
      create_box_title(
        title = "Zimmern\u00e4chte",
        input = input,
        input_id = "infoZimmernaechteSGBS",
        popover_content = "Anzahl der Zimmern\u00e4chte, die die G\u00e4ste (inkl. Kinder) in einem Hotelleriebetrieb verbringen. Zimmern\u00e4chte lassen sich nicht nach Herkunftsland ausweisen und stehen nur f\u00fcr das Total zur Verf\u00fcgung."
      )
    })
    output$title_veraenderung_ankuenfte <- renderUI({
      tags$b(paste("Ver\u00e4nderung Ank\u00fcnfte ", input$referenzjahr, " - ", input$beobachtungsjahr))
    })
    output$title_veraenderung_logiernaechte <- renderUI({
      tags$b(paste("Ver\u00e4nderung Logiern\u00e4chte ", input$referenzjahr, " - ", input$beobachtungsjahr))
    })
    output$title_veraenderung_zimmernaechte <- renderUI({
      tags$b(paste("Ver\u00e4nderung Zimmern\u00e4chte ", input$referenzjahr, " - ", input$beobachtungsjahr))
    })
    output$title_entwicklung_ankuenfte <- renderUI({
      tags$b(paste("Ank\u00fcnfte ", get_start_tl(input$referenzjahr, input$beobachtungsjahr)," - ", get_end_tl(input$referenzjahr, input$beobachtungsjahr)))
    })
    output$title_entwicklung_logiernaechte <- renderUI({
      tags$b(paste("Logiern\u00e4chte ", get_start_tl(input$referenzjahr, input$beobachtungsjahr)," - ", get_end_tl(input$referenzjahr, input$beobachtungsjahr)))
    })
    output$title_entwicklung_dur_stay <- renderUI({
      tags$b(paste("Aufenthaltsdauer (in Tagen) ", get_start_tl(input$referenzjahr, input$beobachtungsjahr)," - ", get_end_tl(input$referenzjahr, input$beobachtungsjahr)))
    })
    output$title_entwicklung_zimmernaechte <- renderUI({
      tags$b(paste("Zimmern\u00e4chte ", get_start_tl(input$referenzjahr, input$beobachtungsjahr)," - ", get_end_tl(input$referenzjahr, input$beobachtungsjahr)))
    })
    output$box_ankuenfte <- eventReactive(c(input$markt, input$beobachtungsregion, input$referenzregion, input$referenzjahr, df_box_beobachtung(), df_box_referenz()), {
      if (input$markt[[1]] == "Total"){
        create_box(
          input = input,
          df_box_beobachtung = df_box_beobachtung(),
          df_box_referenz = df_box_referenz(),
          indicator = "Ankuenfte"
        )
      } else {paste0("<p><b style='color: #cd0e2d';>","In development.</p>")}
    })
    output$box_logiernaechte <- eventReactive(c(input$markt, input$beobachtungsregion, input$referenzregion, input$referenzjahr, df_box_beobachtung(), df_box_referenz()), {
      if (input$markt[[1]] == "Total"){
        create_box(
          input = input,
          df_box_beobachtung = df_box_beobachtung(),
          df_box_referenz = df_box_referenz(),
          indicator = "Logiernaechte"
        )
      } else {paste0("<p><b style='color: #cd0e2d';>","In development.</p>")}
    })
    output$box_dur_stay <- eventReactive(c(input$markt, input$beobachtungsregion, input$referenzregion, input$referenzjahr, df_box_beobachtung(), df_box_referenz()), {
      if (input$markt[[1]] == "Total"){
        create_box_dur_stay(
          input = input,
          df_box_beobachtung = df_box_beobachtung(),
          df_box_referenz = df_box_referenz(),
          indicator = "dur_stay"
        )
      } else {paste0("<p><b style='color: #cd0e2d';>","In development.</p>")}
    })
    output$box_zimmernaechte <- eventReactive(c(input$markt, input$beobachtungsregion, input$referenzregion, input$referenzjahr, df_box_beobachtung(), df_box_referenz()), {
      if (input$markt[[1]] == "Total"){
        create_box(
          input = input,
          df_box_beobachtung = df_box_beobachtung(),
          df_box_referenz = df_box_referenz(),
          indicator = "Zimmernaechte"
        )
      } else {paste0("<p><b style='color: #cd0e2d';>","Zimmern\u00e4chte lassen sich nicht nach Herkunftsland ausweisen und stehen nur f\u00fcr das Total zur Verf\u00fcgung.</p>")}
    })
    output$veraenderung_ankuenfte <- renderEcharts4r({
      req(df_beobachtungsregion(), df_referenzregion(), months_selected())
      plot_bar_change(
        df_beobachtungsregion = df_beobachtungsregion(),
        df_referenzregion = df_referenzregion(),
        variables = .data[[get_country_vars(input$markt)[2]]],
        input = input,
        months_selected = months_selected()
      )
    })
    output$veraenderung_logiernaechte <- renderEcharts4r({
      req(df_beobachtungsregion(), df_referenzregion(), months_selected())
      plot_bar_change(
        df_beobachtungsregion = df_beobachtungsregion(),
        df_referenzregion = df_referenzregion(),
        variables = .data[[get_country_vars(input$markt)[3]]],
        input = input,
        months_selected = months_selected()
      )
    })
    output$veraenderung_zimmernaechte <- renderEcharts4r({
      req(df_beobachtungsregion(), df_referenzregion(), months_selected())
      if (input$markt[[1]] == "Total"){
        plot_bar_change(
          df_beobachtungsregion = df_beobachtungsregion(),
          df_referenzregion = df_referenzregion(),
          variables = Zimmernaechte,
          input = input,
          months_selected = months_selected()
        )
      } else {}
    })
    output$entwicklung_ankuenfte <- renderEcharts4r({
      req(df_beobachtungsregion(), df_referenzregion())

      df_beobachtung <- expand_grid(Jahr = c(min(min(df_beobachtungsregion()$Jahr), max(df_beobachtungsregion()$Jahr - 10)):max(df_beobachtungsregion()$Jahr)), Monat = 1:12, Aggregat = unique(df_beobachtungsregion()$Aggregat)) |>
        # show at least 10 years
        left_join(sgtourism::df_ueberblick) |>
        filter(!is.na(Betriebe))|>
        mutate(region_type = "beobachtungsregion") |>
        group_by(region_type, Jahr) |>
        summarise(Ankuenfte_total = sum(.data[[get_country_vars(input$markt)[2]]], na.rm = TRUE)) |>
        ungroup()

      df_referenz <- expand_grid(Jahr = c(min(min(df_referenzregion()$Jahr), max(df_referenzregion()$Jahr - 10)):max(df_referenzregion()$Jahr)), Monat = 1:12, Aggregat = unique(df_referenzregion()$Aggregat)) |>
        left_join(sgtourism::df_ueberblick) |>
        filter(!is.na(Betriebe))|>
        mutate(region_type = "referenzregion") |>
        group_by(region_type, Jahr) |>
        summarise(Ankuenfte_total = sum(.data[[get_country_vars(input$markt)[2]]], na.rm = TRUE)) |>
        ungroup()

      df <- df_beobachtung |>
        bind_rows(df_referenz) |>
        filter(Jahr <= get_end_tl(input$beobachtungsjahr, input$referenzjahr)) |>
        mutate(region_type = recode(region_type, "beobachtungsregion" = input$beobachtungsregion, "referenzregion" = input$referenzregion)) |>
        mutate(Jahr = as.factor(Jahr))|>
        pivot_wider(names_from = region_type, values_from = Ankuenfte_total)

      if (length(names(df)) == 3) {
        plot_line_multiple(df, "rein")
      } else if (length(names(df)) == 2) {
        plot_line_single(df, "rein")
      }

    })
    output$entwicklung_logiernaechte <- renderEcharts4r({
      req(df_beobachtungsregion(), df_referenzregion())

      df_beobachtung <- expand_grid(Jahr = c(min(min(df_beobachtungsregion()$Jahr), max(df_beobachtungsregion()$Jahr - 10)):max(df_beobachtungsregion()$Jahr)), Monat = 1:12, Aggregat = unique(df_beobachtungsregion()$Aggregat)) |>
        left_join(sgtourism::df_ueberblick) |>
        filter(!is.na(Betriebe))|>
        mutate(region_type = "beobachtungsregion") |>
        group_by(region_type, Jahr) |>
        summarise(Logiernaechte_total = sum(.data[[get_country_vars(input$markt)[3]]], na.rm = TRUE)) |>
        ungroup()

      df_referenz <- expand_grid(Jahr = c(min(min(df_referenzregion()$Jahr), max(df_referenzregion()$Jahr - 10)):max(df_referenzregion()$Jahr)), Monat = 1:12, Aggregat = unique(df_referenzregion()$Aggregat)) |>
        left_join(sgtourism::df_ueberblick) |>
        filter(!is.na(Betriebe))|>
        mutate(region_type = "referenzregion") |>
        group_by(region_type, Jahr) |>
        summarise(Logiernaechte_total = sum(.data[[get_country_vars(input$markt)[3]]], na.rm = TRUE)) |>
        ungroup()

      df <- df_beobachtung |>
        bind_rows(df_referenz) |>
        filter(Jahr <= get_end_tl(input$beobachtungsjahr, input$referenzjahr)) |>
        mutate(region_type = recode(region_type, "beobachtungsregion" = input$beobachtungsregion, "referenzregion" = input$referenzregion)) |>
        mutate(Jahr = as.factor(Jahr))|>
        pivot_wider(names_from = region_type, values_from = Logiernaechte_total)

      if (length(names(df)) == 3) {
        plot_line_multiple(df, "rein")
      } else if (length(names(df)) == 2) {
        plot_line_single(df, "rein")
      }
    })
    output$entwicklung_dur_stay <- renderEcharts4r({
      req(df_beobachtungsregion(), df_referenzregion())

      df_beobachtung <- expand_grid(Jahr = c(min(min(df_beobachtungsregion()$Jahr), max(df_beobachtungsregion()$Jahr - 10)):max(df_beobachtungsregion()$Jahr)), Monat = 1:12, Aggregat = unique(df_beobachtungsregion()$Aggregat)) |>
        left_join(sgtourism::df_ueberblick) |>
        filter(!is.na(Betten))|>
        mutate(region_type = "beobachtungsregion") |>
        group_by(region_type, Jahr) |>
        summarise(dur_stay = round(sum(.data[[get_country_vars(input$markt)[3]]], na.rm = TRUE)/ sum(.data[[get_country_vars(input$markt)[2]]], na.rm = TRUE), 2)) |>
        ungroup()

      df_referenz <- expand_grid(Jahr = c(min(min(df_referenzregion()$Jahr), max(df_referenzregion()$Jahr - 10)):max(df_referenzregion()$Jahr)), Monat = 1:12, Aggregat = unique(df_referenzregion()$Aggregat)) |>
        left_join(sgtourism::df_ueberblick) |>
        filter(!is.na(Betten))|>
        mutate(region_type = "referenzregion") |>
        group_by(region_type, Jahr) |>
        summarise(dur_stay = round(sum(.data[[get_country_vars(input$markt)[3]]], na.rm = TRUE)/ sum(.data[[get_country_vars(input$markt)[2]]], na.rm = TRUE), 2)) |>
        ungroup()

      df <- df_beobachtung |>
        bind_rows(df_referenz) |>
        filter(Jahr <= get_end_tl(input$beobachtungsjahr, input$referenzjahr)) |>
        mutate(region_type = recode(region_type, "beobachtungsregion" = input$beobachtungsregion, "referenzregion" = input$referenzregion)) |>
        mutate(Jahr = as.factor(Jahr))|>
        pivot_wider(names_from = region_type, values_from = dur_stay)

      if (length(names(df)) == 3) {
        plot_line_multiple(df, "rein") |>
          e_y_axis(formatter = e_axis_formatter(digits = 1)
          )
      } else if (length(names(df)) == 2) {
        plot_line_single(df, "rein")|>
          e_y_axis(formatter = e_axis_formatter(digits = 1)
          )
      }
    })

    output$entwicklung_zimmernaechte <- renderEcharts4r({
      req(df_beobachtungsregion(), df_referenzregion())

      if (input$markt[[1]] == "Total"){
        df_beobachtung <- expand_grid(Jahr = c(min(min(df_beobachtungsregion()$Jahr), max(df_beobachtungsregion()$Jahr - 10)):max(df_beobachtungsregion()$Jahr)), Monat = 1:12, Aggregat = unique(df_beobachtungsregion()$Aggregat)) |>
          left_join(sgtourism::df_ueberblick) |>
          filter(!is.na(Betten))|>
          mutate(region_type = "beobachtungsregion") |>
          group_by(region_type, Jahr) |>
          summarise(Zimmernaechte = sum(Zimmernaechte, na.rm = TRUE)) |>
          ungroup()

        df_referenz <- expand_grid(Jahr = c(min(min(df_referenzregion()$Jahr), max(df_referenzregion()$Jahr - 10)):max(df_referenzregion()$Jahr)), Monat = 1:12, Aggregat = unique(df_referenzregion()$Aggregat)) |>
          left_join(sgtourism::df_ueberblick) |>
          filter(!is.na(Betten))|>
          mutate(region_type = "referenzregion") |>
          group_by(region_type, Jahr) |>
          summarise(Zimmernaechte = sum(Zimmernaechte, na.rm = TRUE)) |>
          ungroup()

        df <- df_beobachtung |>
          bind_rows(df_referenz) |>
          filter(Jahr <= get_end_tl(input$beobachtungsjahr, input$referenzjahr)) |>
          mutate(region_type = recode(region_type, "beobachtungsregion" = input$beobachtungsregion, "referenzregion" = input$referenzregion)) |>
          mutate(Jahr = as.factor(Jahr))|>
          pivot_wider(names_from = region_type, values_from = Zimmernaechte)

        if (length(names(df)) == 3) {
          plot_line_multiple(df, "rein") |>
            e_y_axis(formatter = e_axis_formatter(digits = 1)
            )
        } else if (length(names(df)) == 2) {
          plot_line_single(df, "rein")|>
            e_y_axis(formatter = e_axis_formatter(digits = 1)
            )
        }
      }
      else {}
    })


    # Occupancy tab -----------------------------------------------------------
    output$title_bed_occ <- renderUI({
      create_box_title(
        title = "Bettenauslastung (brutto)",
        input = input,
        input_id = "infobed_occSGBS",
        popover_content = "Anzahl Logiern\u00e4chte geteilt durch die Bruttobettenkapazit\u00e4t in der betreffenden Periode. Die Bruttobettenkapazit\u00e4t eines Betriebes entspricht der Anzahl Betten im Erhebungszeitraum multipliziert mit der Anzahl Tage dieses Zeitraums."
      )
    })
    output$title_bed_net_occ <- renderUI({
      create_box_title(
        title = "Bettenauslastung (netto)",
        input = input,
        input_id = "infobed_occSGBS",
        popover_content = "Anzahl Logiern\u00e4chte geteilt durch die Nettobettenkapazit\u00e4t in der betreffenden Periode. Die Nettobettenkapazit\u00e4t eines Betriebes entspricht der Anzahl Betten im Erhebungszeitraum multipliziert mit der Anzahl \u00d6ffnungstage in diesem Zeitraum."
      )
    })
    output$title_room_occ <- renderUI({
      create_box_title(
        title = "Zimmerauslastung (brutto)",
        input = input,
        input_id = "inforoom_occSGBS",
        popover_content = "Anzahl Zimmern\u00e4chte geteilt durch die Bruttozimmerkapazit\u00e4t in der betreffenden Periode. Die Bruttozimmerkapazit\u00e4t eines Betriebes entspricht der Anzahl Zimmer im Erhebungszeitraum multipliziert mit der Anzahl Tage dieses Zeitraums."
      )
    })
    output$title_room_net_occ <- renderUI({
      create_box_title(
        title = "Zimmerauslastung (netto)",
        input = input,
        input_id = "inforoom_net_occSGBS",
        popover_content = "Anzahl Zimmern\u00e4chte geteilt durch die Nettozimmerkapazit\u00e4t in der betreffenden Periode. Die Nettobettenkapazit\u00e4t eines Betriebes entspricht der Anzahl Zimmer im Erhebungszeitraum multipliziert mit der Anzahl \u00d6ffnungstage in diesem Zeitraum."
      )
    })
    output$title_entw_kurz_bed_occ <- renderUI({
      tags$b(paste("Bettenauslastung (brutto) ", input$beobachtungsregion))
    })
    output$title_entw_kurz_bed_net_occ <- renderUI({
      tags$b(paste("Bettenauslastung (netto) ", input$beobachtungsregion))
    })
    output$title_entw_kurz_room_occ <- renderUI({
      tags$b(paste("Zimmerauslastung (brutto) ", input$beobachtungsregion))
    })
    output$title_entwicklung_bed_occ <- renderUI({
      tags$b(paste("Bettenauslastung (brutto) ", get_start_tl(input$referenzjahr, input$beobachtungsjahr)," - ", get_end_tl(input$referenzjahr, input$beobachtungsjahr)))
    })
    output$title_entwicklung_bed_net_occ <- renderUI({
      tags$b(paste("Bettenauslastung (netto) ", get_start_tl(input$referenzjahr, input$beobachtungsjahr)," - ", get_end_tl(input$referenzjahr, input$beobachtungsjahr)))
    })
    output$title_entwicklung_room_occ <- renderUI({
      tags$b(paste("Zimmerauslastung (brutto) ", get_start_tl(input$referenzjahr, input$beobachtungsjahr)," - ", get_end_tl(input$referenzjahr, input$beobachtungsjahr)))
    })
    output$box_bed_occ <- eventReactive(c(input$beobachtungsregion, input$referenzregion, input$referenzjahr, df_box_beobachtung(), df_box_referenz()), {
      create_box_pct(
        input = input,
        df_box_beobachtung = df_box_beobachtung(),
        df_box_referenz = df_box_referenz(),
        indicator = "bed_occ"
      )
    })
    output$box_bed_net_occ <- eventReactive(c(input$beobachtungsregion, input$referenzregion, input$referenzjahr, df_box_beobachtung(), df_box_referenz()), {
      create_box_pct(
        input = input,
        df_box_beobachtung = df_box_beobachtung(),
        df_box_referenz = df_box_referenz(),
        indicator = "bed_net_occ"
      )
    })
    output$box_room_occ <- eventReactive(c(input$beobachtungsregion, input$referenzregion, input$referenzjahr, df_box_beobachtung(), df_box_referenz()), {
      create_box_pct(
        input = input,
        df_box_beobachtung = df_box_beobachtung(),
        df_box_referenz = df_box_referenz(),
        indicator = "room_occ"
      )
    })
    output$box_room_net_occ <- eventReactive(c(input$beobachtungsregion, input$referenzregion, input$referenzjahr, df_box_beobachtung(), df_box_referenz()), {
      create_box_pct(
        input = input,
        df_box_beobachtung = df_box_beobachtung(),
        df_box_referenz = df_box_referenz(),
        indicator = "room_net_occ"
      )
    })

    output$entw_kurz_bed_occ <- renderEcharts4r({
      req(input$region, months_selected(), df_beobachtungsregion())

      df <- df_beobachtungsregion() |>
        filter(Monat %in% months_selected()) |>
        mutate(Monat = month_abb_de()[Monat]) |> # month names
        mutate(Jahr = as.factor(Jahr)) |>
        group_by(Jahr, Monat) |>
        summarise(bed_occ = sum(Logiernaechte) / sum(Bettenmonat)) |>
        ungroup() |>
        complete(Monat = month_abb_de(), nesting(Jahr), fill = list(bed_occ = NA)) |>
        pivot_wider(names_from = Jahr, values_from = bed_occ) |>
        mutate(Monat = factor(Monat, levels = month_abb_de())) |>
        arrange(Monat)

      if (length(names(df)) == 3) {
        plot_line_multiple(df, "intensiv") |>
          e_y_axis(formatter = e_axis_formatter("percent", digits = 0))
        } else if (length(names(df)) == 2) {
        plot_line_single(df, "intensiv") |>
          e_y_axis(formatter = e_axis_formatter("percent", digits = 0))
        }
    })
    output$entw_kurz_bed_net_occ <- renderEcharts4r({
      req(input$region, months_selected(),  df_beobachtungsregion())

      df <- df_beobachtungsregion() |>
        filter(Monat %in% months_selected()) |>
        mutate(Monat = month_abb_de()[Monat]) |> # month names
        mutate(Jahr = as.factor(Jahr)) |>
        group_by(Jahr, Monat) |>
        summarise(bed_net_occ = sum(Logiernaechte) / sum(NettoZimm)) |>
        ungroup() |>
        complete(Monat = month_abb_de(), nesting(Jahr), fill = list(bed_net_occ = NA)) |>
        pivot_wider(names_from = Jahr, values_from = bed_net_occ) |>
        mutate(Monat = factor(Monat, levels = month_abb_de())) |>
        arrange(Monat)

      if (length(names(df)) == 3) {
        plot_line_multiple(df, "intensiv") |>
          e_y_axis(formatter = e_axis_formatter("percent", digits = 0))
      } else if (length(names(df)) == 2) {
        plot_line_single(df, "intensiv") |>
          e_y_axis(formatter = e_axis_formatter("percent", digits = 0))
      }
    })
    output$entw_kurz_room_occ <- renderEcharts4r({
      req(input$region, months_selected(), df_beobachtungsregion())

      df <- df_beobachtungsregion() |>
        filter(Monat %in% months_selected()) |>
        mutate(Monat = month_abb_de()[Monat]) |> # month names
        mutate(Jahr = as.factor(Jahr)) |>
        group_by(Jahr, Monat) |>
        summarise(room_occ = sum(Zimmernaechte) / sum(Zimmermonat)) |>
        ungroup() |>
        complete(Monat = month_abb_de(), nesting(Jahr), fill = list(room_occ = NA)) |>
        pivot_wider(names_from = Jahr, values_from = room_occ) |>
        mutate(Monat = factor(Monat, levels = month_abb_de())) |>
        arrange(Monat)

      if (length(names(df)) == 3) {
        plot_line_multiple(df, "intensiv") |>
          e_y_axis(formatter = e_axis_formatter("percent", digits = 0))
      } else if (length(names(df)) == 2) {
        plot_line_single(df, "intensiv") |>
          e_y_axis(formatter = e_axis_formatter("percent", digits = 0))
      }
    })

    output$entwicklung_bed_occ <- renderEcharts4r({
      req(input$region, df_beobachtungsregion(), df_referenzregion())

      df_beobachtung <- expand_grid(Jahr = c(min(min(df_beobachtungsregion()$Jahr), max(df_beobachtungsregion()$Jahr - 10)):max(df_beobachtungsregion()$Jahr)), Monat = 1:12, Aggregat = unique(df_beobachtungsregion()$Aggregat)) |>
        # show at least 10 years
        left_join(sgtourism::df_ueberblick) |>
        filter(!is.na(Betriebe))|>
        mutate(region_type = "beobachtungsregion") |>
        group_by(region_type, Jahr) |>
        summarise(bed_occ = sum(Logiernaechte) / sum(Bettenmonat)) |>
        ungroup()

      df_referenz <- expand_grid(Jahr = c(min(min(df_referenzregion()$Jahr), max(df_referenzregion()$Jahr - 10)):max(df_referenzregion()$Jahr)), Monat = 1:12, Aggregat = unique(df_referenzregion()$Aggregat)) |>
        left_join(sgtourism::df_ueberblick) |>
        filter(!is.na(Betriebe))|>
        mutate(region_type = "referenzregion") |>
        group_by(region_type, Jahr) |>
        summarise(bed_occ = sum(Logiernaechte) / sum(Bettenmonat)) |>
        ungroup()

      df <- df_beobachtung |>
        bind_rows(df_referenz) |>
        filter(Jahr <= get_end_tl(input$beobachtungsjahr, input$referenzjahr)) |>
        mutate(region_type = recode(region_type, "beobachtungsregion" = input$beobachtungsregion, "referenzregion" = input$referenzregion)) |>
        mutate(Jahr = as.factor(Jahr))|>
        pivot_wider(names_from = region_type, values_from = bed_occ)

      if (length(names(df)) == 3) {
        plot_line_multiple(df, "rein") |>
          e_y_axis(formatter = e_axis_formatter("percent", digits = 0)
                   )
      } else if (length(names(df)) == 2) {
        plot_line_single(df, "rein")|>
          e_y_axis(formatter = e_axis_formatter("percent", digits = 0)
          )
      }

    })
    output$entwicklung_bed_net_occ <- renderEcharts4r({
      req(input$region, df_beobachtungsregion(), df_referenzregion())

      df_beobachtung <- expand_grid(Jahr = c(min(min(df_beobachtungsregion()$Jahr), max(df_beobachtungsregion()$Jahr - 10)):max(df_beobachtungsregion()$Jahr)), Monat = 1:12, Aggregat = unique(df_beobachtungsregion()$Aggregat)) |>
        # show at least 10 years
        left_join(sgtourism::df_ueberblick) |>
        filter(!is.na(Betriebe))|>
        mutate(region_type = "beobachtungsregion") |>
        group_by(region_type, Jahr) |>
        summarise(bed_net_occ = sum(Logiernaechte) / sum(NettoBett)) |>
        ungroup()

      df_referenz <- expand_grid(Jahr = c(min(min(df_referenzregion()$Jahr), max(df_referenzregion()$Jahr - 10)):max(df_referenzregion()$Jahr)), Monat = 1:12, Aggregat = unique(df_referenzregion()$Aggregat)) |>
        left_join(sgtourism::df_ueberblick) |>
        filter(!is.na(Betriebe))|>
        mutate(region_type = "referenzregion") |>
        group_by(region_type, Jahr) |>
        summarise(bed_net_occ = sum(Logiernaechte) / sum(NettoBett)) |>
        ungroup()

      df <- df_beobachtung |>
        bind_rows(df_referenz) |>
        filter(Jahr <= get_end_tl(input$beobachtungsjahr, input$referenzjahr)) |>
        mutate(region_type = recode(region_type, "beobachtungsregion" = input$beobachtungsregion, "referenzregion" = input$referenzregion)) |>
        mutate(Jahr = as.factor(Jahr))|>
        pivot_wider(names_from = region_type, values_from = bed_net_occ)

      if (length(names(df)) == 3) {
        plot_line_multiple(df, "rein") |>
          e_y_axis(formatter = e_axis_formatter("percent", digits = 0)
          )
      } else if (length(names(df)) == 2) {
        plot_line_single(df, "rein")|>
          e_y_axis(formatter = e_axis_formatter("percent", digits = 0)
          )
      }

    })

    output$entwicklung_room_occ <- renderEcharts4r({
      req(input$region, df_beobachtungsregion(), df_referenzregion())

      df_beobachtung <- expand_grid(Jahr = c(min(min(df_beobachtungsregion()$Jahr), max(df_beobachtungsregion()$Jahr - 10)):max(df_beobachtungsregion()$Jahr)), Monat = 1:12, Aggregat = unique(df_beobachtungsregion()$Aggregat)) |>
        left_join(sgtourism::df_ueberblick) |>
        filter(!is.na(Betten))|>
        mutate(region_type = "beobachtungsregion") |>
        group_by(region_type, Jahr) |>
        summarise(room_occ = sum(Zimmernaechte) / sum(Zimmermonat)) |>
        ungroup()

      df_referenz <- expand_grid(Jahr = c(min(min(df_referenzregion()$Jahr), max(df_referenzregion()$Jahr - 10)):max(df_referenzregion()$Jahr)), Monat = 1:12, Aggregat = unique(df_referenzregion()$Aggregat)) |>
        left_join(sgtourism::df_ueberblick) |>
        filter(!is.na(Betten)) |>
        mutate(region_type = "referenzregion") |>
        group_by(region_type, Jahr) |>
        summarise(room_occ = sum(Zimmernaechte) / sum(Zimmermonat)) |>
        ungroup()

      df <- df_beobachtung |>
        bind_rows(df_referenz) |>
        filter(Jahr <= get_end_tl(input$beobachtungsjahr, input$referenzjahr)) |>
        mutate(region_type = recode(region_type, "beobachtungsregion" = input$beobachtungsregion, "referenzregion" = input$referenzregion)) |>
        mutate(Jahr = as.factor(Jahr))|>
        pivot_wider(names_from = region_type, values_from = room_occ)

      if (length(names(df)) == 3) {
        plot_line_multiple(df, "rein") |>
          e_y_axis(formatter = e_axis_formatter("percent", digits = 0)
          )
      } else if (length(names(df)) == 2) {
        plot_line_single(df, "rein")|>
          e_y_axis(formatter = e_axis_formatter("percent", digits = 0)
          )
        }
      })
  })
  }

## To be copied in the UI
# mod_details_tab_ui("details_tab_1")

## To be copied in the server
# mod_details_tab_server("details_tab_1")




# Ich habe folgende Abfrage: if(input$referenzregion == "Kanton St.Gallen") {
#   sgtourism::df_ueberblick |>
#     filter(Kt == 17) |>
#     filter(Jahr == input$beobachtungsjahr) |>
#     mutate(Referenz = "beobachtungsjahr") |>
#     bind_rows(
#       sgtourism::df_ueberblick |>
#         filter(Kt == 17) |>
#         filter(Jahr == input$referenzjahr) |>
#         mutate(Referenz = "referenzjahr")
#       Ich möchte diese so abändern, dass die Variable input$referenzregion in einer lookup-Tabelle in Spalte 2 (aggregat_tx) nachgeschaut wird und für die Variable Kt der Wert aus Spalte 1 (aggregat_cd) der lookup-Tabelle übernommen wird.
#
#       # Lookup-Tabelle
#       lookup_table <- data.frame(
#         aggregat_cd = c(17, 18),  # Beispielwerte für Kt
#         aggregat_tx = c("Kanton St.Gallen", "Kanton Zürich")  # Beispielwerte für referenzregion
#       )
#
#       # Geänderte Abfrage
#       if (input$referenzregion %in% lookup_table$aggregat_tx) {
#         kt_value <- lookup_table$aggregat_cd[lookup_table$aggregat_tx == input$referenzregion]
#
#         sgtourism::df_ueberblick |>
#           filter(Kt == kt_value) |>
#           filter(Jahr == input$beobachtungsjahr) |>
#           mutate(Referenz = "beobachtungsjahr") |>
#           bind_rows(
#             sgtourism::df_ueberblick |>
#               filter(Kt == kt_value) |>
#               filter(Jahr == input$referenzjahr) |>
#               mutate(Referenz = "referenzjahr")
#           )
#       }
