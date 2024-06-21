#' Shiny Module Server for Contrast Panel
#'
#' @param id identifier for shiny reactive
#' @param main_par parameters from calling modules
#' @param traitSignal,traitStats static data frames
#' @param customSettings list of custom settings
#' @param keepDatatraits keep datatraits if not `NULL`
#'
#' @return reactive object 
#' @importFrom shiny column moduleServer NS observeEvent
#'             reactive renderUI req selectInput tagList uiOutput
#'             updateSelectInput
#' @importFrom DT renderDataTable
#' @importFrom foundr conditionContrasts
#' @export
#'
contrastTableServer <- function(id, main_par,
                               traitSignal, traitStats,
                               customSettings = NULL,
                               keepDatatraits = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    stats_table <- traitOrderServer("shinyOrder", main_par,
      traitStats, customSettings, keepDatatraits)
    
    ###############################################################
    
    shiny::reactive({
      shiny::req(stats_table())
      
      foundr::conditionContrasts(traitSignal, stats_table(), 
        termname = stats_table()$term[1], rawStats = traitStats)
    }, label = "stats_table")
  })
}
#' Shiny Module UI for Trait Panel
#' @return nothing returned
#' @rdname contrastTableServer
#' @export
contrastTableUI <- function(id) {
  ns <- shiny::NS(id)
  traitOrderUI(ns("shinyOrder"))
}
#' Shiny Module App for Trait Panel
#' @return nothing returned
#' @rdname contrastTableServer
#' @export
contrastTableApp <- function(id) {
  title <- "Shiny Contrast Table"
  
  ui <- function() {
    
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          mainParInput("main_par"),
          shiny::uiOutput("sex")
        ),
        
        shiny::mainPanel(
          shiny::tagList(
            contrastTableUI("contrast_table"),
            shiny::h3("Contrasts"),
            shiny::uiOutput("table")))
      ))
  }
  
  server <- function(input, output, session) {
    
    # MODULE
    # Contrast Module Table
    main_par <- mainParServer("main_par", traitStats)
    contrast_table <- contrastTableServer("contrast_table", input, main_par,
      traitSignal, traitStats, customSettings)
    
    # SERVER-SIDE INPUTS
    sexes <- c(B = "Both Sexes", F = "Female", M = "Male", C = "Sex Contrast")
    output$sex <- shiny::renderUI({
      shiny::selectInput("sex", "", as.vector(sexes))
    })

    # Output Table
    output$table <- shiny::renderUI({
      shiny::req(contrast_table(), main_par$dataset, input$sex)
      tbl <- dplyr::filter(contrast_table(), sex %in% input$sex)
      DT::renderDataTable(foundr::summary_conditionContrasts(tbl, ntrait = 0))
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
