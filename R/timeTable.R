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
    
    # OUTPUTS
    # traitTimeData
    
    # Identify all Time Traits.
    timetrait_all <- foundr::timetraitsall(traitSignal)
    
    # Subset Stats to time traits.
    time_trait_table <- time_trait_subset(traitStats, timetrait_all)
    
    # MODULES
    # Order Traits by Stats.
    stats_table <- traitOrderServer("shinyOrder", panel_par, main_par,
                                   time_trait_table, customSettings)
    
    # Identify Time Traits.
    time_trait_names <- timeTraitsServer("time_trait_names", panel_par, main_par,
                                        traitSignal, stats_table)
    
    ###############################################################
    
    shiny::reactive({
      shiny::req(time_trait_names$traits, time_trait_names$time,
                 time_trait_names$response, stats_table())
      
      foundr::traitTimes(traitData, traitSignal, traitStats,
                 time_trait_names$traits, time_trait_names$time,
                 time_trait_names$response,
                 strains = panel_par$strains)
    }, label = "traitTimes")
  })
}
#' Shiny Input for Times Table
#' @return nothing returned
#' @rdname timeTableServer
#' @export
timeTableInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(4, traitOrderInput(ns("shinyOrder"))), # Order
    shiny::column(8, timeTraitsInput(ns("time_trait_names")))) # Traits
}
#' Shiny UI for Times Table
#' @return nothing returned
#' @rdname timeTableServer
#' @export
timeTableUI <- function(id) {
  ns <- shiny::NS(id)
  timeTraitsUI(ns("time_trait_names")) # Time Unit
}
#' Shiny Output for Times Table
#' @return nothing returned
#' @rdname timeTableServer
#' @export
timeTableOutput <- function(id) {
  ns <- shiny::NS(id)
  timeTraitsOutput(ns("time_trait_names")) # Response
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
            shiny::column(3, mainParInput("main_par")),
            shiny::column(9, timeTableInput("time_table"))),
          timeTableUI("time_table"),
          
          timeTableOutput("time_table")
        ),
        shiny::mainPanel(
          shiny::h3("Time Table"),
          shiny::uiOutput("time_table")
        )
      )
    )
  }
  
  server <- function(input, output, session) {
    # MODULES
    main_par <- mainParServer("main_par", traitStats)
    time_table <- timeTableServer("time_table", input, main_par,
      traitData, traitSignal, traitStats)
    
    output$time_table <- shiny::renderUI({
      shiny::req(time_table())
      DT::renderDataTable(
        time_table()$stats[[1]],
        escape = FALSE,
        options = list(scrollX = TRUE, pageLength = 10))
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
