#' Shiny Module Server for Times Plots
#'
#' @param id identifier for shiny reactive
#' @param main_par reactive arguments 
#' @param traitData,traitSignal,traitStats static objects
#'
#' @return nothing returned
#' @importFrom shiny moduleServer NS
#' @export
#'
timePanelServer <- function(id, main_par,
                           traitData, traitSignal, traitStats) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # *** Want Times panel to look more like Traits panel.
    # Move Response (from shinyTimeTraitsOutput) to main from side.
    # Put Order and Traits (from shinyTraitOrderInput and shinyTimeTraitsInput) up with datasets.
    # Keep Time unit (from shinyTimeTraitsUI) on side panel.
    # Key Datasets and Trait.
    # shinyTimePanel
    # Mostly works but lost shinyTimeTraitsUI and time unit.
    
    
    # INPUTS
    # passed inputs:
    #   main_par$dataset
    #   main_par$height
    #   input$strains
    #   input$facet
    
    # MODULES
    timeOutput <- timeTableServer("shinyTimeTable", input, main_par, 
                                  traitData, traitSignal, traitStats)
    timePlotServer("shinyTimePlot", input, main_par, traitSignal, timeOutput)
    
    # SERVER-SIDE Inputs
    output$strains <- shiny::renderUI({
      choices <- names(foundr::CCcolors)
      shiny::checkboxGroupInput(ns("strains"), "Strains",
                                choices = choices, selected = choices, inline = TRUE)
    })
  })
}
#' Shiny Module Input for Time Panel
#' @return nothing returned
#' @rdname timePanelServer
#' @export
timePanelInput <- function(id) { # 4:Order, 8:Traits
  ns <- shiny::NS(id)
  timeTableInput(ns("shinyTimeTable"))
}
#' Shiny Module UI for Time Panel
#' @return nothing returned
#' @rdname timePanelServer
#' @export
timePanelUI <- function(id) { # Time Unit
  ns <- shiny::NS(id)
  timeTableUI(ns("shinyTimeTable"))
}
#' Shiny Module Output for Times Plot
#' @return nothing returned
#' @rdname timePanelServer
#' @export
timePanelOutput <- function(id) { # Response; Plots or Tables
  ns <- shiny::NS(id)
  shiny::tagList(
    timePlotUI(ns("shinyTimePlot")),
    shiny::fluidRow(
      shiny::column(6, timeTableOutput(ns("shinyTimeTable"))), # Response
      shiny::column(6, timePlotInput(ns("shinyTimePlot")))),
    shiny::fluidRow(
      shiny::column(9, shiny::uiOutput(ns("strains"))),
      shiny::column(3, shiny::checkboxInput(ns("facet"), "Facet by strain?", TRUE))),
    timePlotOutput(ns("shinyTimePlot"))) # Plots or Tables
}
#' Shiny Module App for Times Plot
#' @return nothing returned
#' @rdname timePanelServer
#' @export
timePanelApp <- function() {
  title <- "Test shinyTime Module"
  
  ui <- function() {
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::fluidRow(
            shiny::column(3, shiny::uiOutput("dataset")),
            shiny::column(9, timePanelInput("shinyTimePanel"))),
          timePanelUI("shinyTimePanel"),
          
          shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1),
        ),
        
        shiny::mainPanel(
          timePanelOutput("shinyTimePanel")
        )))
  }
  
  server <- function(input, output, session) {
    # MODULES
    timePanelServer("shinyTimePanel", input, traitData, traitSignal, traitStats)
    
    # SERVER-SIDE INPUTS
    output$dataset <- shiny::renderUI({
      # Dataset selection.
      datasets <- unique(traitStats$dataset)
      # Get datasets.
      shiny::selectInput("dataset", "Datasets:",
                         datasets, datasets[1], multiple = TRUE)
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}

