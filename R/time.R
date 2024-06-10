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
timeServer <- function(id, main_par,
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
    time_table <- timeTableServer("shinyTimeTable", input, main_par, 
                                  traitData, traitSignal, traitStats)
    timePlotServer("shinyTimePlot", input, main_par, traitSignal, time_table)
    
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
#' @rdname timeServer
#' @export
timeInput <- function(id) { # 4:Order, 8:Traits
  ns <- shiny::NS(id)
  timeTableInput(ns("shinyTimeTable"))
}
#' Shiny Module UI for Time Panel
#' @return nothing returned
#' @rdname timeServer
#' @export
timeUI <- function(id) { # Time Unit
  ns <- shiny::NS(id)
  timeTableUI(ns("shinyTimeTable"))
}
#' Shiny Module Output for Times Plot
#' @return nothing returned
#' @rdname timeServer
#' @export
timeOutput <- function(id) { # Response; Plots or Tables
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
#' @rdname timeServer
#' @export
timeApp <- function() {
  title <- "Test shinyTime Module"
  
  ui <- function() {
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::fluidRow(
            shiny::column(3, mainParInput("main_par")),
            shiny::column(9, timeInput("time"))),
          timeUI("time"),
          
          mainParUI("main_par"),
        ),
        
        shiny::mainPanel(
          timeOutput("time")
        )))
  }
  
  server <- function(input, output, session) {
    # MODULES
    main_par <- mainParServer("main_par", traitStats)
    timeServer("time", main_par, traitData, traitSignal, traitStats)
  }
  
  shiny::shinyApp(ui = ui, server = server)
}

