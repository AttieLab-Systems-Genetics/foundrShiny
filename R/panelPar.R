#' Shiny Server for panelPar Server
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
panelParServer <- function(id, main_par, traitStats = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$strains <- shiny::renderUI({
      choices <- names(foundr::CCcolors)
      shiny::checkboxGroupInput("strains", "Strains",
                                choices = choices, selected = choices, inline = TRUE)
    })
    output$traits <- shiny::renderUI({
      traits <- foundr::unite_datatraits(
        dplyr::distinct(
          dplyr::filter(traitStats, .data$dataset %in% main_par$dataset),
          .data$dataset, .data$trait))
      shiny::selectInput("trait","Traits:", traits)
    })
    sexes <- c(B = "Both Sexes", F = "Female", M = "Male", C = "Sex Contrast")
    output$sex <- shiny::renderUI({
      shiny::selectInput("sex", "", as.vector(sexes))
    })
    ######################################################################
    input
  })
}
#' @export
#' @rdname panelParServer
panelParInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(9, shiny::uiOutput(ns("strains"))),
    shiny::column(3, shiny::checkboxInput(ns("facet"),
                                          "Facet by strain?", TRUE)))
}
#' @export
#' @rdname panelParServer
panelParUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("traits"))
}
#' @export
#' @rdname panelParServer
panelParOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("sex"))
}
#' @param title title of app
#' @export
#' @rdname panelParServer
panelParApp <- function(title = "") {
  ui <- shiny::bootstrapPage(
    mainParInput("main_par"), # dataset
    shiny::h3("panel_par parameters"),
    shiny::h4("panelParInput: strains, facet"),
    panelParInput("panel_par"), # strains, facet
    shiny::h4("panelParUI: traits"),
    panelParUI("panel_par"), # Traits
    shiny::h4("panelParOutput: sex"),
    panelParOutput("panel_par") # Sexes (B/F/M/C)
  )
  server <- function(input, output, session) {
    main_par <- mainParServer("main_par", traitStats)
    panelParServer("panel_par", main_par, traitStats)
  }
  shiny::shinyApp(ui, server)
}
