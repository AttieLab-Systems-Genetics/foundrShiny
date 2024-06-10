#' Shiny Server for dataset INput
#'
#' @param id identifier for shiny reactive
#' @param traitStats static object
#' @return reactive input
#' 
#' @export
#' @importFrom shiny bootstrapPage moduleServer NS observeEvent reactiveVal 
#'             renderUI req selectInput shinyApp sliderInput uiOutput
#'
datasetServer <- function(id, traitStats = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    #    input$dataset
    #    input$height

    # SERVER-SIDE INPUTS
    output$dataset <- shiny::renderUI({
      # Dataset selection.
      datasets <- unique(traitStats$dataset)
      selected <- data_selection()
      
      # Get datasets.
      shiny::selectInput(ns("dataset"), "Datasets:", datasets, selected,
                         multiple = TRUE)
    })
    data_selection <- shiny::reactiveVal(unique(traitStats$dataset)[1],
                                         label = "data_selection")
    shiny::observeEvent(input$dataset, data_selection(input$dataset))
    ######################################################################
    input
  })
}
#' @export
#' @rdname datasetServer
datasetInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("dataset"))
}
#' @export
#' @rdname datasetServer
datasetUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::sliderInput(ns("height"), "Plot height (in):", 3, 10, 6, step = 1)
}
#' @param title title of app
#' @export
#' @rdname datasetServer
datasetApp <- function(title = "") {
  ui <- shiny::bootstrapPage(
    datasetInput("dataset"), 
    datasetUI("dataset")
  )
  server <- function(input, output, session) {
    datasetServer("dataset", traitStats)
  }
  shiny::shinyApp(ui, server)
}
