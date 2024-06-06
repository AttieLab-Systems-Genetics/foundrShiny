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
    
    # INPUTS
    # contrastSex inputs
    #   main_par$tabpanel
    #   main_par$height
    #   main_par$strains
    #   panel_par$sex
    
    # RETURNS
    
    # Contrast Trait Plots
    contrastPlotServer("contrast_plot",
                      panel_par, main_par,
                      contrastTable, customSettings, 
                      shiny::reactive("Sex Contrasts"))
  })
}
#' Shiny Sex Input for Contrast Plots
#'
#' @return nothing returned
#' @rdname contrastSexServer
#' @export
contrastSexInput <- function(id) {
  ns <- shiny::NS(id)
  contrastPlotInput(ns("contrast_plot"))
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
  # Read trait data.
  source(system.file(file.path("shinyApp", "LiverData.R"), package = "foundrShiny"))
  
  title <- "Test Shiny Module"
  
  ui <- function() {
    
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::uiOutput("dataset"),
          contrastTableInput("contrast_table")),
        
        shiny::mainPanel(
          shiny::tagList(
            contrastSexInput("sex_plot"),
            shiny::fluidRow(
              shiny::column(4, shiny::uiOutput("sex")),
              shiny::column(8, contrastSexUI("sex_plot"))),
            contrastSexOutput("sex_plot")
          )
        )
      ))
  }
  
  server <- function(input, output, session) {
    
    # *** need persistent module choice (reactiveVal)
    # *** table from traits()
    # *** sliders from Volcano
    # *** simplify using traitModule as below
    # *** move module choice to side panel
    
    # MODULE
    # Contrast Trait Table
    contrastOutput <- contrastTableServer("contrast_table",
                                         input, input, traitSignal, traitStats, customSettings)
    # Contrast Modules.
    moduleOutput <- contrastSexServer("sex_plot", input, input, traitContrPval, traitModule)
    
    traitContrPval <- reactive({
      shiny::req(contrastOutput())
      
      pvalue <- attr(traitModule, "p.value") # set by construction of `traitModule`
      if(is.null(pvalue)) pvalue <- 1.0
      
      dplyr::filter(shiny::req(contrastOutput()), .data$p.value <= pvalue)
    })
    
    # SERVER-SIDE INPUTS
    output$dataset <- shiny::renderUI({
      # Dataset selection.
      datasets <- unique(traitStats$dataset)
      
      # Get datasets.
      shiny::selectInput("dataset", "Datasets:",
                         datasets, datasets[1], multiple = TRUE)
    })
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
    
    output$intro <- renderUI({
      shiny::renderText("intro", {
        paste("Guideline is to have power of 6 and size of 4 for unsigned modules.")
      })
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
