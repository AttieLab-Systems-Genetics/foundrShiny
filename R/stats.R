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
    
    # *** allow to flip from SD to p.value
    # *** fix all the kludgey stuff--may need to refactor
    # *** contrastPlot: add scatterplot
    # *** rename as contrast now has generic contrast/stat
    # *** refactor data so Charles can use
    
    # INPUTS
    # Main inputs:
    #   main_par$height
    #   main_par$dataset
    # Stats inputs: (see output$tab_volcano below)
    #   input$term
    #   input$traitnames
    #   input$interact
    #   input$volsd
    #   input$volpval
    
    # MODULES
    # Contrast Trait Plots
    contrastPlotServer("contrast_plot", input, main_par,
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
  })
}
#' Shiny Module Output for Stats Plot
#' @return nothing returned
#' @rdname statsServer
#' @export
statsOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    contrastPlotInput(ns("contrast_plot")),
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
        shiny::uiOutput("dataset"),
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1)),
      
      shiny::mainPanel(
        statsOutput("StatsPanel")
      )
    )
  )
  
  server <- function(input, output, session) {
    # SERVER-SIDE INPUTS
    output$dataset <- shiny::renderUI({
      # Dataset selection.
      datasets <- unique(traitStats$dataset)
      
      # Get new input parameters for Stats.
      shiny::selectInput("dataset", "Datasets:",
                         datasets, datasets[1], multiple = TRUE)
    })
    statsServer("StatsPanel", input, traitStats)
  }
  
  shiny::shinyApp(ui = ui, server = server)}
