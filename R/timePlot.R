#' Shiny Module Server for Time Plots
#'
#' @param id identifier for shiny reactive
#' @param panel_par,main_par reactive arguments 
#' @param traitSignal static object
#' @param traitTimesData reactive object
#' @param responses possible types of responses
#'
#' @return nothing returned
#' @importFrom shiny column fluidRow h3 moduleServer NS observeEvent plotOutput
#'             radioButtons reactive reactiveVal reactiveValues renderPlot
#'             renderUI req selectInput selectizeInput tagList uiOutput
#'             updateSelectizeInput
#' @importFrom DT renderDataTable
#' @importFrom stringr str_remove str_replace_all
#' @importFrom foundr ggplot_traitTimes timetraitsall
#' @export
#'
timePlotServer <- function(id, panel_par, main_par,
                          traitSignal, traitTimesData) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$plots <- shiny::renderUI({
      shiny::req(timeplots(), timestats())
      
      shiny::tagList(
        shiny::h3("Plot over Time"),
        shiny::plotOutput(ns("timeplots"),
                          height = paste0(main_par$height, "in")),
        shiny::h3("Plot of Time Summaries"),
        shiny::plotOutput(ns("timestats"),
                          height = paste0(main_par$height, "in")))
    })
    output$timeplots <- shiny::renderPlot(print(timeplots()))
    output$timestats <- shiny::renderPlot(print(timestats()))
    
    timeplots <- shiny::reactive({
      shiny::req(traitTimesData(), panel_par$strains)
      
      foundr::ggplot_traitTimes(traitTimesData()$traits, facet_strain = panel_par$facet)
    }, label = "timeplots")
    timestats <- shiny::reactive({
      shiny::req(traitTimesData())
      
      foundr::ggplot_traitTimes(traitTimesData()$stats)
    }, label = "timestats")
    
    ###############################################################
    shiny::reactive({
      list(Traits = shiny::req(timeplots()),
           Stats = shiny::req(timestats()))
    })
  })
}
#' Shiny Module Output for Time Plots
#' @return nothing returned
#' @rdname timePlotServer
#' @export
timePlotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("plots"))
}
#' Shiny Module App for Times Plot
#' @return nothing returned
#' @rdname timeServer
#' @export
timePlotApp <- function() {
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
          mainParOutput("main_par"), # plot_table, height
        ),
        
        shiny::mainPanel(
          panelParInput("panel_par"), # strains, facet
          timePlotOutput("time_plot")
        )))
  }
  
  server <- function(input, output, session) {
    main_par <- mainParServer("main_par", traitStats)
    panel_par <- panelParServer("panel_par", main_par, traitStats)
    time_table <- timeTableServer("time_table", panel_par, main_par,
                                  traitData, traitSignal, traitStats)
    timePlotServer("time_plot", panel_par, main_par, traitSignal, time_table)
  }
  
  shiny::shinyApp(ui = ui, server = server)
}

