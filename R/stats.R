#' Shiny Module Server for Stats Plots
#'
#' @param id identifier for shiny reactive
#' @param main_par reactive arguments from `server()`
#' @param traitStats static data frame
#' @param customSettings list of custom settings
#' @param facet facet on `strain` if `TRUE`
#'
#' @return reactive object for `statsOutput`
#' @importFrom shiny column fluidRow moduleServer NS observeEvent plotOutput
#'             reactive renderPlot renderUI req selectInput selectizeInput
#'             sliderInput tagList uiOutput updateSelectInput
#' @importFrom plotly plotlyOutput ggplotly renderPlotly
#' @importFrom ggplot2 ylim
#' @importFrom rlang .data
#' @export
#'
statsServer <- function(id, main_par, traitStats, customSettings = NULL,
                             facet = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    panel_par <- panelParServer("panel_par", main_par, traitStats)
    # Contrast Trait Plots
    contrast_list <- contrastPlotServer("contrast_plot", panel_par, main_par,
      traitStatsSelected, customSettings, shiny::reactive("Stats Contrasts"))
    
    # Dataset selection.
    data_selection <- shiny::reactiveVal(NULL, label = "data_selection")
    shiny::observeEvent(main_par$dataset,
                        data_selection(main_par$dataset))
    
    # Stats for selected datasets.
    traitStatsSelected <- shiny::reactive({
      shiny::req(data_selection())
      
      dplyr::filter(traitStats, .data$dataset %in% data_selection())
    })
    ###############################################################
    contrast_list
  })
}
#' Shiny Module Output for Stats Plot
#' @return nothing returned
#' @rdname statsServer
#' @export
statsOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    panelParUI(ns("panel_par")), # sex (B/F/M/C)
    contrastPlotUI(ns("contrast_plot")),
    contrastPlotOutput(ns("contrast_plot")))
}
#' Shiny Module App for Stats Plot
#' @return nothing returned
#' @rdname statsServer
#' @export
statsApp <- function() {
  title <- "Test Shiny Stats Module"
  
  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        mainParInput("main_par"), # dataset
        mainParUI("main_par"), # order
        border_line(),
        mainParOutput("main_par"), # plot_table, height
        downloadOutput("download")
      ),
      shiny::mainPanel(
        statsOutput("StatsPanel")
      )
    )
  )
  
  server <- function(input, output, session) {
    main_par <- mainParServer("main_par", traitStats)
    statsServer("StatsPanel", main_par, traitStats)
  }
  
  shiny::shinyApp(ui = ui, server = server)}
