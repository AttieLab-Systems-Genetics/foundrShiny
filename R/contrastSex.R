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
contrastSexServer <- function(id, panel_par, main_par,
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
#' @rdname contrastSexServer
#' @export
contrastSexUI <- function(id) {
  ns <- shiny::NS(id)
  contrastPlotUI(ns("contrast_plot"))
}
#' Shiny Sex Output for Contrast Plots
#'
#' @return nothing returned
#' @rdname contrastSexServer
#' @export
contrastSexOutput <- function(id) {
  ns <- shiny::NS(id)
  contrastPlotOutput(ns("contrast_plot"))
}
#' Shiny Sex App for Contrast Plots
#'
#' @return nothing returned
#' @rdname contrastSexServer
#' @export
contrastSexApp <- function() {
  title <- "Test contrastSex Module"
  
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
            shiny::column(4, shiny::uiOutput("sex")),
            shiny::column(8, contrastSexUI("sex_plot"))),
          contrastSexOutput("sex_plot")
        )
      )
    )
  }
  
  server <- function(input, output, session) {
    # Main parameters
    main_par <- mainParServer("main_par", traitStats)
    # Contrast Trait Table
    contrast_table <- contrastTableServer("contrast_table", main_par,
      traitSignal, traitStats, customSettings)
    # Contrast List
    contrast_list <- contrastSexServer("sex_plot", input, main_par,
      contrast_table, traitModule)

    # SERVER-SIDE INPUTS
    output$strains <- shiny::renderUI({
      choices <- names(foundr::CCcolors)
      shiny::checkboxGroupInput(
        "strains", "Strains",
        choices = choices, selected = choices, inline = TRUE)
    })
    sexes <- c(B = "Both Sexes", F = "Female", M = "Male", C = "Sex Contrast")
    output$sex <- shiny::renderUI({
      shiny::selectInput("sex", "", as.vector(sexes))
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
