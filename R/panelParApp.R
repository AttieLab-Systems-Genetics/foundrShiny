#' Panel Parameters App
#'
#' @param id identifier for shiny reactive
#' @param main_par main parameters
#' @param traitStats static object
#' @param panel_name name of panel
#' @return reactive input
#' 
#' @importFrom shiny bootstrapPage h4 moduleServer NS observeEvent radioButtons
#'             reactiveVal reactiveValues renderUI req selectInput shinyApp
#'             sliderInput uiOutput
#' @export
panelParApp <- function() {
  ui <- shiny::bootstrapPage(
    shiny::h3("panel_par parameters"),
    shiny::h4("panelParInput: strains, facet"),
    panelParInput("panel_par"), # strains, facet
    shiny::h4("panelParUI: sex"),
    panelParUI("panel_par"), # sex (B/F/M/C)
    shiny::h4("panelParOutput: height or table"),
    shiny::fluidRow(
      shiny::column(6, mainParOutput1("main_par")), # plot_table
      shiny::column(6, panelParOutput("panel_par"))) # height or table
  )
  server <- function(input, output, session) {
    main_par <- mainParServer("main_par", traitStats)
    panelParServer("panel_par", main_par, traitStats, "trait")
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname panelParApp
panelParServer <- function(id, main_par, traitStats = NULL, panel_name = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$strains <- shiny::renderUI({
      choices <- names(foundr::CCcolors)
      shiny::checkboxGroupInput(ns("strains"), "Strains",
        choices = choices, selected = choices, inline = TRUE)
    })
    sexes <- c(B = "Both Sexes", F = "Female", M = "Male", C = "Sex Contrast")
    output$sex <- shiny::renderUI({
      shiny::selectInput(ns("sex"), "", as.vector(sexes))
    })
    output$table <- shiny::renderUI({
      if(shiny::req(main_par$plot_table) == "Plots") {
        shiny::sliderInput(ns("height"), "Plot height (in):", 3, 10, 6,
                           step = 1) # height
      } else { # Tables
        if(panel_name %in% c("trait", "time")) {
          table_names <- c("Cell Means","Stats")
          if(panel_name == "trait")
            table_names <- c(table_names, "Correlations")
          shiny::radioButtons(ns("table"), "Download:",
                              table_names, "Cell Means", inline = TRUE)
        }
      }
    })
    ######################################################################
    input
  })
}
#' @export
#' @rdname panelParApp
panelParInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(9, shiny::uiOutput(ns("strains"))),
    shiny::column(3, shiny::checkboxInput(ns("facet"),
                                          "Facet by strain?", TRUE)))
}
#' @export
#' @rdname panelParApp
panelParUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("sex")) # sex
}
#' @export
#' @rdname panelParApp
panelParOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("table")) # height or table
}
