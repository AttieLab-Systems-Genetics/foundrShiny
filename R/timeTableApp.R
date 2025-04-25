#' Times Tables App
#'
#' @param id identifier for shiny reactive
#' @param panel_par,main_par reactive arguments 
#' @param traitData,traitSignal,traitStats static objects
#' @return nothing returned
#'
#' @importFrom shiny column fluidRow h3 observeEvent moduleServer NS plotOutput
#'             reactive reactiveVal renderPlot renderUI req selectInput
#'             selectizeInput tagList uiOutput updateSelectizeInput
#' @importFrom shiny column fluidRow NS
#' @importFrom foundr timetraitsall traitTimes
#' @importFrom DT renderDataTable
#' @export
timeTableApp <- function() {
  title <- "Test shinyTime Module"
  ui <- function() {
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::fluidRow(
            shiny::column(3, mainParInput("main_par")), # dataset
            shiny::column(9, timeTableInput("time_table"))), # traits
          timeTableUI("time_table"), # time_units, response
        ),
        shiny::mainPanel(
          panelParInput("panel_par"), # strains, facet
          timeTableOutput("time_table")
        )
      )
    )
  }
  server <- function(input, output, session) {
    main_par <- mainParServer("main_par", traitStats)
    panel_par <- panelParServer("panel_par", main_par, traitStats)
    time_table <- timeTableServer("time_table", panel_par, main_par,
                                  traitData, traitSignal, traitStats)
  }
  shiny::shinyApp(ui = ui, server = server)
}
#' @rdname timeTableApp
#' @export
timeTableServer <- function(id, panel_par, main_par,
                           traitData, traitSignal, traitStats) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Subset Stats to time traits.
    time_trait_table <- time_trait_subset(traitStats,
      foundr::timetraitsall(traitSignal))
    # Order Traits by Stats.
    stats_table <- traitOrderServer("stats_table", main_par,
      time_trait_table, customSettings)
    # Identify Time Traits.
    time_traits <- timeTraitsServer("time_traits",
      panel_par, main_par, traitSignal, stats_table)
    
    # Create time table.
    time_table <- shiny::reactive({
      shiny::req(time_traits$traits, time_traits$time, time_traits$response)
      foundr::traitTimes(traitData, traitSignal, traitStats,
        time_traits$traits, time_traits$time, time_traits$response,
        strains = panel_par$strains)
    }, label = "time_table")
    
    # Render time table.
    output$time_table <- shiny::renderUI({
      shiny::req(time_table())
      shiny::tagList(
        shiny::h3("Cell Means"),
        DT::renderDataTable(summary_traitTime(time_table()),
          escape = FALSE, options = list(scrollX = TRUE, pageLength = 10)),
        shiny::h3("Stats: p.value"),
        DT::renderDataTable(stats_time_table(time_table()$stats),
          escape = FALSE, options = list(scrollX = TRUE, pageLength = 10)))
    })
    
    ###############################################################
    time_table
  })
}
#' @rdname timeTableApp
#' @export
timeTableInput <- function(id) {
  ns <- shiny::NS(id)
  timeTraitsInput(ns("time_traits")) # traits
}
#' @rdname timeTableApp
#' @export
timeTableUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    timeTraitsUI(ns("time_traits")), # time_units
    timeTraitsOutput(ns("time_traits")) # response
  )
}
#' @rdname timeTableApp
#' @export
timeTableOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("time_table"))
}
