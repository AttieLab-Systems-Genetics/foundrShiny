#' Shiny Module Server for Contrast Panel
#'
#' @param id identifier for shiny reactive
#' @param panel_par,main_par parameters from calling modules
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
contrastTableServer <- function(id, panel_par, main_par,
                               traitSignal, traitStats,
                               customSettings = NULL, keepDatatraits = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # shinyContrastTable inputs
    #   main_par$tabpanel
    # RETURNS
    #   contrastTable()
    
    # MODULES
    # Order Traits by Stats.
    orderOutput <- traitOrderServer("shinyOrder", panel_par, main_par,
                                    traitStats, customSettings, keepDatatraits)
    
    ###############################################################
    
    shiny::reactive({
      shiny::req(orderOutput())
      
      foundr::conditionContrasts(traitSignal, orderOutput(), 
                         termname = orderOutput()$term[1], rawStats = traitStats)
    }, label = "contrastTable")
  })
}
#' Shiny Module Input for Trait Panel
#' @return nothing returned
#' @rdname contrastTableServer
#' @export
contrastTableInput <- function(id) {
  ns <- shiny::NS(id)
  traitOrderInput(ns("shinyOrder"))
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
  # Read trait data.
  source(system.file(file.path("shinyApp", "LiverData.R"), package = "foundrShiny"))
  
  title <- "Shiny Module Contrast Table"
  
  ui <- function() {
    
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::uiOutput("dataset"),
          shiny::uiOutput("sex"),
          contrastTableInput("shinyContrastTable"),
          shiny::uiOutput("module")
        ),
        
        shiny::mainPanel(
          shiny::tagList(
            contrastTableUI("shinyContrastTable"),
            shiny::h3("Contrasts"),
            shiny::uiOutput("table")))
      ))
  }
  
  server <- function(input, output, session) {
    
    # MODULE
    # Contrast Module Table
    tableContrast <- contrastTableServer("shinyContrastTable", input, input,
                                         traitSignal, traitStats, customSettings, keepDatatraits)
    
    # SERVER-SIDE INPUTS
    output$dataset <- shiny::renderUI({
      # Dataset selection.
      datasets <- unique(traitStats$dataset)
      
      # Get datasets.
      shiny::selectInput("dataset", "Datasets:",
                         datasets, datasets[1], multiple = TRUE)
    })
    sexes <- c(B = "Both Sexes", F = "Female", M = "Male", C = "Sex Contrast")
    output$sex <- shiny::renderUI({
      shiny::selectInput("sex", "", as.vector(sexes))
    })
    
    datamodule <- shiny::reactive({
      traitModule[shiny::req(input$dataset)]
    })
    output$module <- shiny::renderUI({
      shiny::selectizeInput("module", "Module:", NULL)
    })
    shiny::observeEvent(
      shiny::req(datamodule(), input$sex, input$dataset),
      {
        if(foundr:::is_sex_module(datamodule())) {
          sextraits <- NULL
        } else {
          sextraits <- unique(datamodule()[[input$dataset]]$value$modules$module)
        }
        shiny::updateSelectizeInput(session, "module", choices = sextraits,
                                    selected = "", server = TRUE)
      })
    keepDatatraits <- reactive({
      shiny::req(input$dataset, datamodule())
      if(foundr:::is_sex_module(datamodule()) || !shiny::isTruthy(input$module))
        return(NULL)
      foundr:::keptDatatraits(traitModule, input$dataset[1], input$module)
    })
    
    # Output Table
    output$table <- shiny::renderUI({
      shiny::req(tableContrast(), input$dataset, input$sex)
      tbl <- dplyr::filter(tableContrast(), sex %in% input$sex)
      if(foundr:::is_sex_module(datamodule())) {
        tbl <- dplyr::filter(tbl, dataset %in% input$dataset)
      }
      DT::renderDataTable(foundr::summary_conditionContrasts(tbl, ntrait = 0))
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
