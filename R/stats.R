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
    panel_par <- panelParServer("panel_par", main_par, traitStats, "stats")
    trait_stats <- shiny::reactive({
      shiny::req(main_par$dataset)
      dplyr::filter(traitStats, .data$dataset %in% main_par$dataset)
    })
    stats_list <- contrastPlotServer("contrast_plot", panel_par, main_par,
      trait_stats, customSettings, shiny::reactive("Stats Contrasts"))
    stats_list$panel <- shiny::reactive("Stats")
    stats_list$height <- shiny::reactive(panel_par$height)
    ###############################################################
    stats_list
  })
}
#' @rdname statsServer
#' @export
statsUI <- function(id) { # height or table
  ns <- shiny::NS(id)
  panelParOutput(ns("panel_par")) # height or table
}
#' Shiny Module Output for Stats Plot
#' @return nothing returned
#' @rdname statsServer
#' @export
statsOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    contrastPlotUI(ns("contrast_plot")), # ordername, interact
    contrastPlotOutput(ns("contrast_plot"))) # volsd, volvert, rownames
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
        border_line(),
        shiny::fluidRow(
          shiny::column(6, mainParOutput1("main_par")), # plot_table
          shiny::column(6, statsUI("stats_list"))), # height or table
        downloadOutput("download")
      ),
      shiny::mainPanel(
        statsOutput("stats_list")
      )
    )
  )
  
  server <- function(input, output, session) {
    main_par <- mainParServer("main_par", traitStats)
    stats_list <- statsServer("stats_list", main_par, traitStats)
    downloadServer("download", "Stats", main_par, stats_list)
  }
  
  shiny::shinyApp(ui = ui, server = server)}
