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
panelParServer <- function(id, main_par, traitSignal = NULL) {
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
          dplyr::filter(traitSignal, .data$dataset %in% main_par$dataset),
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
panelParStrains <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("strains"))
}
#' @export
#' @rdname panelParServer
panelParTraits <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("traits"))
}
#' @export
#' @rdname panelParServer
panelParSex <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("sex"))
}
#' @param title title of app
#' @export
#' @rdname panelParServer
panelParApp <- function(title = "") {
  ui <- shiny::bootstrapPage(
    mainParInput("main_par"),
    shiny::h3("panel_par parameters"),
    shiny::h4("panelParStrains: strains"),
    panelParStrains("panel_par"), 
    shiny::h4("panelParTraits: traits"),
    panelParTraits("panel_par"), 
    shiny::h4("panelParSex: sex"),
    panelParSex("panel_par")
  )
  server <- function(input, output, session) {
    main_par <- mainParServer("main_par", traitSignal)
    panelParServer("panel_par", main_par, traitSignal)
  }
  shiny::shinyApp(ui, server)
}
