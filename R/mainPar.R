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
    
    # Dataset selection.
    datasets <- unique(traitStats$dataset)
    output$dataset <- shiny::renderUI({
      selected <- input$dataset
      shiny::selectInput(ns("dataset"), "Datasets:", datasets, selected,
                         multiple = TRUE)
    })

    # Order Criteria for Trait Names
    output$order <- shiny::renderUI({
      selected <- input$order
      choices <- order_choices(traitStats)
      shiny::selectInput(ns("order"), "Order traits by", choices, selected)
    })
    ######################################################################
    input
  })
}
#' @export
#' @rdname mainParServer
mainParInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("dataset")) # dataset
}
#' @export
#' @rdname mainParServer
mainParUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("order")) # order
}
#' @export
#' @rdname mainParServer
mainParOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(6, mainParOutput1(id)), # plot_table
    shiny::column(6, mainParOutput2(id))) # height
}
mainParOutput1 <- function(id) { # plot_table
  ns <- shiny::NS(id)
  shiny::radioButtons(ns("plot_table"), "", c("Plots","Tables"), "Plots",
                                         inline = TRUE)
}
mainParOutput2 <- function(id) {
  ns <- shiny::NS(id)
  shiny::sliderInput(ns("height"), "Plot height (in):", 3, 10, 6, step = 1)
}
#' @param title title of app
#' @export
#' @rdname mainParServer
mainParApp <- function(title = "") {
  ui <- shiny::bootstrapPage(
    shiny::h3("main_par parameters"),
    shiny::h4("mainParInput: dataset"),
    mainParInput("main_par"), # dataset
    shiny::h4("mainParUI: order"),
    mainParUI("main_par"), # order
    shiny::h4("mainParOutput: plot_table, height"),
    mainParOutput("main_par") # plot_table, height
  )
  
  server <- function(input, output, session) {
    mainParServer("main_par", traitStats)
  }
  shiny::shinyApp(ui, server)
}
