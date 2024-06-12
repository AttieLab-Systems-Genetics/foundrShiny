#' Shiny Server for mainPar Server
#'
#' @param id identifier for shiny reactive
#' @param traitStats static object
#' @return reactive input
#' 
#' @export
#' @importFrom shiny bootstrapPage h4 moduleServer NS observeEvent radioButtons
#'             reactiveVal reactiveValues renderUI req selectInput shinyApp
#'             sliderInput uiOutput
#'
mainParServer <- function(id, traitStats = NULL) {
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
    
    output$butshow <- shiny::renderUI({
      if(input$butshow == "Plots") {
        shiny::sliderInput(ns("height"), "Plot height (in):", 3, 10, 6, step = 1)
      }
    })
    ######################################################################
    input
  })
}
#' @export
#' @rdname mainParServer
mainParInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("dataset"))
}
#' @export
#' @rdname mainParServer
mainParUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(6, shiny::radioButtons(ns("butshow"), "", c("Plots","Tables"), "Plots",
                                         inline = TRUE)),
    shiny::column(6, shiny::uiOutput(ns("butshow")))
  )
}
#' @param title title of app
#' @export
#' @rdname mainParServer
mainParApp <- function(title = "") {
  ui <- shiny::bootstrapPage(
    shiny::h4("main_par$dataset"),
    mainParInput("main_par"), 
    shiny::h4("main_par$height"),
    mainParUI("main_par"),
    shiny::h4("main_par$butshow"),
    mainParOutput("main_par")
  )
  server <- function(input, output, session) {
    mainParServer("main_par", traitStats)
  }
  shiny::shinyApp(ui, server)
}
