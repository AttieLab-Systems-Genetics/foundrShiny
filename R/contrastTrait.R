#' Shiny Sex Server for Contrast Plots
#'
#' @param id identifier for shiny reactive
#' @param panel_par,main_par input parameters
#' @param contrastTable reactive data frame
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny column fluidRow moduleServer NS observeEvent
#'             reactive renderUI req selectInput tagList uiOutput
#'             updateSelectInput
#' @importFrom DT renderDataTable
#' @export
#'
contrastTraitServer <- function(id, panel_par, main_par,
                             contrastTable, customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Contrast Trait Plots
    contrastPlotServer("contrast_plot",
                      panel_par, main_par,
                      contrastTable, customSettings, 
                      shiny::reactive("Sex Contrasts"))
  })
}
#' Shiny Sex UI for Contrast Plots
#'
#' @return nothing returned
#' @rdname contrastTraitServer
#' @export
contrastTraitUI <- function(id) {
  ns <- shiny::NS(id)
  contrastPlotUI(ns("contrast_plot")) # ordername, interact
}
#' Shiny Sex Output for Contrast Plots
#'
#' @return nothing returned
#' @rdname contrastTraitServer
#' @export
contrastTraitOutput <- function(id) {
  ns <- shiny::NS(id)
  contrastPlotOutput(ns("contrast_plot")) # volsd, volvert, rownames
}
#' Shiny Sex App for Contrast Plots
#'
#' @return nothing returned
#' @rdname contrastTraitServer
#' @export
contrastTraitApp <- function() {
  title <- "Test contrastTrait Module"
  
  ui <- function() {
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          mainParInput("main_par"), # dataset
          border_line(),
          mainParOutput("main_par") # plot_table, height
        ),
        shiny::mainPanel(
          shiny::fluidRow(
            shiny::column(4, panelParUI("panel_par")), # sex
            shiny::column(8, contrastTraitUI("contrast_list"))), # ordername, interact
          contrastTraitOutput("contrast_list")
        )
      )
    )
  }
  
  server <- function(input, output, session) {
    # Main parameters
    main_par <- mainParServer("main_par", traitStats)
    panel_par <- panelParServer("panel_par", main_par, traitStats)
    # Contrast Trait Table
    contrast_table <- contrastTableServer("contrast_table", main_par,
      traitSignal, traitStats, customSettings)
    # Contrast List
    contrast_list <- contrastTraitServer("contrast_list", panel_par, main_par,
      contrast_table, traitModule)
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
