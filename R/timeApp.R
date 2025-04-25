#' Times Panel App
#'
#' @param id identifier for shiny reactive
#' @param main_par reactive arguments 
#' @param traitData,traitSignal,traitStats static objects
#' @return nothing returned
#'
#' @importFrom shiny column fluidPage mainPanel moduleServer NS renderUI req
#'.            shinyApp sidebarPanel tagList titlePanel uiOutput
#' @export
timeApp <- function() {
  title <- "Test shinyTime Module"
  ui <- function() {
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::fluidRow(
            shiny::column(6, mainParInput("main_par")), # dataset
            shiny::column(6, mainParUI("main_par"))), # order
          timeInput("time"), # key_trait, time_unit, response
          border_line(),
          shiny::fluidRow(
            shiny::column(6, mainParOutput1("main_par")), # plot_table
            shiny::column(6, timeUI("time"))), # height or table
          downloadOutput("download")
        ),
        shiny::mainPanel(
          timeOutput("time")
        )))
  }
  server <- function(input, output, session) {
    # MODULES
    main_par <- mainParServer("main_par", traitStats)
    time_list <- timeServer("time", main_par,
                            traitData, traitSignal, traitStats)
    downloadServer("download", "Trait", main_par, time_list)
  }
  shiny::shinyApp(ui = ui, server = server)
}
#' @rdname timeApp
#' @export
timeServer <- function(id, main_par,
                           traitData, traitSignal, traitStats) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    panel_par <- panelParServer("panel_par", main_par, traitStats, "time")
    time_table <- timeTableServer("time_table", panel_par, main_par, 
      traitData, traitSignal, traitStats)
    time_list <- timePlotServer("time_plot", panel_par, main_par,
      traitSignal, time_table)
    
    output$plot_table <- shiny::renderUI({
      shiny::tagList(
        panelParInput(ns("panel_par")), # strains, facet
        switch(shiny::req(main_par$plot_table),
               Plots  = timePlotOutput(ns("time_plot")),
               Tables = timeTableOutput(ns("time_table")))
      )
    })
    ###############################################################
    time_list
  })
}
#' @rdname timeApp
#' @export
timeInput <- function(id) { # key_trait, time_unit, response
  ns <- shiny::NS(id)
  shiny::tagList(
    timeTableInput(ns("time_table")), # key_trait
    timeTableUI(ns("time_table")) # time_unit, response
  )
}
#' @rdname timeApp
#' @export
timeUI <- function(id) { # height or table
  ns <- shiny::NS(id)
  panelParOutput(ns("panel_par")) # height or table
}
#' @rdname timeApp
#' @export
timeOutput <- function(id) { # Response; Plots or Tables
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("plot_table")))
}
