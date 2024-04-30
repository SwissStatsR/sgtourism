#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import bs4Dash
#' @importFrom shiny tagList HTML modalDialog div fluidRow
#' @importFrom waiter spin_1
#' @importFrom utils packageVersion
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bs4Dash::dashboardPage(
      title = "Tourismus-Dashboard Kanton St.Gallen",
      dark = NULL,
      help = TRUE, # show popover
      preloader = list(html = tagList(waiter::spin_1(), "Loading St.Gallen dashboard..."), color = "#7AB800"),
      header = bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand(
          title = "Tourismus-Dashboard",
          href = "https://sg.ch",
          image = "www/sg_flag.png"
        )
      ),
      sidebar = dashboardSidebar(
        collapsed = FALSE, # if TRUE, collapsed by default to gain space
        skin = "light",
        status = "primary",
        elevation = 3,
        sidebarMenu(
          menuItem(
            text = "\u00dcberblick",
            icon = icon("home"),
            tabName = "ueberblick"
          )
          # ,
          # menuItem(
          #   text = "Details",
          #   icon = icon("map"),
          #   # MAYBE MISSING Ostschweiz ?
          #   # menuSubItem(
          #   #     "Ostschweiz",
          #   #     tabName = "ostschweiz"
          #   # ),
          #   menuSubItem(
          #     "Kanton St.Gallen",
          #     tabName = "st-gallen"
          #   ),
          #   menuSubItem(
          #     "Heidiland",
          #     tabName = "heidiland"
          #   ),
          #   menuSubItem(
          #     "St.Gallen-Bodensee",
          #     tabName = "st-gallen-bodensee"
          #   ),
          #   menuSubItem(
          #     "Toggenburg",
          #     tabName = "toggenburg"
          #   ),
          #   menuSubItem(
          #     "Z\u00fcrichsee",
          #     tabName = "zuerichsee"
          #   )
          # )
        )
      ),
      footer = dashboardFooter(
        left = HTML("Quelle: Bundesamt f\u00fcr Statistik, HESTA (aktuelles Jahr: provisorische Zahlen), Aufbereitung und Berechnung: <a target=_blank href='https://www.statistik.sg.ch'>FfS-SG</a>"),
        right = HTML("version: <a target=_blank href='https://github.com/statistikSG/sgtourism'>", paste0(packageVersion("sgtourism")), "</a>")
      ),
      body = bs4Dash::dashboardBody(
        tabItems(
          tabItem(
            tabName = "ueberblick",
            # modalDialog(
            #   div(
            #     tags$p("ACHTUNG: Dieses Dashboard befindet sich noch in der Entwicklungsphase und dient ausschlie\u00dflich zu Testzwecken. Nutzen Sie die bereitgestellten Funktionen mit Vorsicht, da sich das System in st\u00e4ndiger Weiterentwicklung befindet."),
            #     tags$p("Die hier pr\u00e4sentierten Daten k\u00f6nnen unter Umst\u00e4nden fehlerhaft sein. Bitte ziehen Sie f\u00fcr belastbare Analysen vorerst noch die Auswertungen auf folgender Seite heran:"),
            #     tags$p(tags$a(href="https://www.sg.ch/ueber-den-kanton-st-gallen/statistik/themen/B10/aufenthaltstourismus--hotellerie.html","Webpage Aufenthaltstourismus Hotellerie"))
            #   ),
            #   title = "WORK IN PROGRESS",
            #   size = "l",
            #   easyClose = TRUE
            # ),
            mod_ueberblick_tab_ui("ueberblick_tab_1")
          ),
          tabItem(
            tabName = "st-gallen",
            fluidRow(
              column(
                width = 12,
                bs4Dash::tabsetPanel(type = "tabs",
                            tab("Angebot", box(title = tags$b("Title"), tags$p("Content"))),
                            tab("Nachfrage"),
                            tab("Auslastung"),
                            tab("M\u00e4rkte")
                )
              )
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom shiny tags
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Tourismus-Dashboard Kanton St.Gallen"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
  )
}
