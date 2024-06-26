#' Shiny Server for Times Plots
#'
#' @param id identifier for shiny reactive
#' @param panel_par,main_par reactive arguments 
#' @param traitData,traitSignal,traitStats static objects
#'
#' @return nothing returned
#' @importFrom shiny column fluidRow h3 observeEvent moduleServer NS plotOutput
#'             reactive reactiveVal renderPlot renderUI req selectInput
#'             selectizeInput tagList uiOutput updateSelectizeInput
#' @importFrom shiny column fluidRow NS
#' @importFrom foundr timetraitsall traitTimes
#' @importFrom DT renderDataTable
#' @export
timeTableServer <- function(id, panel_par, main_par,
                           traitData, traitSignal, traitStats) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Order Traits by Stats.
    stats_table <- traitOrderServer("shinyOrder", main_par,
      time_trait_table, customSettings)
    # Subset Stats to time traits.
    time_trait_table <- time_trait_subset(traitStats,
      foundr::timetraitsall(traitSignal))
    # Identify Time Traits.
    time_trait_names <- timeTraitsServer("time_trait_names",
      panel_par, main_par, traitSignal, stats_table)
    
    time_table <- shiny::reactive({
      shiny::req(time_trait_names$traits, time_trait_names$time,
                 time_trait_names$response, stats_table())
      
      foundr::traitTimes(traitData, traitSignal, traitStats,
                 time_trait_names$traits, time_trait_names$time,
                 time_trait_names$response,
                 strains = panel_par$strains)
    }, label = "time_table")
    
    statstable <- shiny::reactive({
      shiny::req(time_table())
      stats_time_table(time_table()$stats)
    }, label = "statstable")
    traitstable <- shiny::reactive({
      shiny::req(time_table())
      summary_traitTime(time_table())
    }, label = "statstable")
    output$time_table <- shiny::renderUI({
      shiny::req(statstable(), traitstable())
      shiny::tagList(
        shiny::h3("Cell Means"),
        DT::renderDataTable(traitstable(), escape = FALSE,
                            options = list(scrollX = TRUE, pageLength = 10)),
        
        shiny::h3("Stats: p.value"),
        DT::renderDataTable(statstable(), escape = FALSE,
                            options = list(scrollX = TRUE, pageLength = 10)))
    })
    
    ###############################################################
    time_table
  })
}
#' Shiny Input for Times Table
#' @return nothing returned
#' @rdname timeTableServer
#' @export
timeTableInput <- function(id) {
  ns <- shiny::NS(id)
  timeTraitsInput(ns("time_trait_names")) # traits
}
#' Shiny UI for Times Table
#' @return nothing returned
#' @rdname timeTableServer
#' @export
timeTableUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    timeTraitsUI(ns("time_trait_names")), # time_units
    timeTraitsOutput(ns("time_trait_names")) # response
  )
}
#' Shiny Output for Times Table
#' @return nothing returned
#' @rdname timeTableServer
#' @export
timeTableOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("time_table"))
}
#' Shiny App for Times Table
#' @return nothing returned
#' @rdname timeTableServer
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
