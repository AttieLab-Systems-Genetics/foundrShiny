#' Shiny Module Server for Contrasts over Time
#'
#' @param id identifier for shiny reactive
#' @param panel_par,main_par reactive arguments 
#' @param traitSignal,traitStats static data frames
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny h3 isTruthy moduleServer NS reactive renderText renderUI
#'             tagList
#' @importFrom stringr str_to_title
#' @export
#'
contrastTimeServer <- function(id, panel_par, main_par,
                              traitSignal, traitStats, contrastTable,
                              customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # MODULES
    # Identify Time Traits.
    times_list <- timeTraitsServer("times_list", panel_par, main_par,
                                        traitSignal, contrastTable)
    
    # Contrast Time Signal
    shiny::reactive({
      shiny::req(contrastTable(), times_list$traits, panel_par$strains,
                 times_list$response, times_list$time)
      
      # Convert `contrastTable()` to a `Signal` style data frame.
      contrastSignal <- contrast_signal(contrastTable())
      
      traitTimes(contrastSignal, contrastSignal, traitStats,
                 times_list$traits, times_list$time,
                 times_list$response, strains = panel_par$strains)
    }, label = "contrastTime")
  })
}
#' Shiny Module Input for Contrasts over Time
#' @return nothing returned
#' @rdname contrastTimeServer
#' @export
contrastTimeInput <- function(id) {
  ns <- shiny::NS(id)
  timeTraitsInput(ns("times_list"))
}
#' Shiny Module UI for Contrasts over Time
#' @return nothing returned
#' @rdname contrastTimeServer
#' @export
contrastTimeUI <- function(id) {
  ns <- shiny::NS(id)
  timeTraitsUI(ns("times_list")) # time_units
}
#' Shiny App for Contrast Times
#' @return nothing returned
#' @rdname contrastTimeServer
#' @export
contrastTimeApp <- function() {
  title <- "Test Contrast Time Module"
  
  ui <- function() {
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::fluidRow(
            shiny::column(3, mainParInput("main_par")), # dataset
            shiny::column(3, mainParUI("main_par")), # order
            shiny::column(6, contrastTimeInput("contrast_time"))), # Traits
          contrastTimeUI("contrast_time"),
          shiny::uiOutput("strains")
        ),
        shiny::mainPanel(
          shiny::h3("Time Table"),
          shiny::uiOutput("contrast_time")
        )
      )
    )
  }
  
  server <- function(input, output, session) {
    main_par <- mainParServer("main_par", traitStatsTime)
    # Contrast Time Trait Table
    stats_time_table <- time_trait_subset(traitStats,
                                          timetraitsall(traitSignal))
    times_table <- contrastTableServer("times_table", main_par,
      traitSignal, stats_time_table, customSettings)
    contrast_time <- contrastTimeServer("contrast_time", input, main_par,
      traitSignal, stats_time_table, times_table)
    
    # SERVER-SIDE Inputs
    output$strains <- shiny::renderUI({
      choices <- names(foundr::CCcolors)
      shiny::checkboxGroupInput("strains", "Strains",
        choices = choices, selected = choices, inline = TRUE)
    })

    # Output    
    output$contrast_time <- shiny::renderUI({
      shiny::req(contrast_time())
      DT::renderDataTable(
        contrast_time()$stats[[1]],
        escape = FALSE,
        options = list(scrollX = TRUE, pageLength = 10))
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
