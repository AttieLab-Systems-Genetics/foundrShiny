#' Shiny Module Server for Time Plots
#'
#' @param id identifier for shiny reactive
#' @param panel_par,main_par reactive arguments 
#' @param traitSignal static object
#' @param time_table reactive object
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
                          traitSignal, time_table) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    timeplots <- shiny::reactive({
      shiny::req(time_table(), panel_par$strains)
      foundr::ggplot_traitTimes(time_table()$traits,
                                facet_strain = panel_par$facet)
    }, label = "timeplots")
    output$timeplots <- shiny::renderPlot(print(timeplots()))
    timestats <- shiny::reactive({
      shiny::req(time_table())
      foundr::ggplot_traitTimes(time_table()$stats)
    }, label = "timestats")
    output$timestats <- shiny::renderPlot(print(timestats()))
    
    output$plots <- shiny::renderUI({
      shiny::req(timeplots(), timestats(), panel_par$height)
      shiny::tagList(
        shiny::h3("Plot over Time"),
        shiny::plotOutput(ns("timeplots"),
                          height = paste0(panel_par$height, "in")),
        shiny::h3("Stats over Time as -log10(p)"),
        shiny::plotOutput(ns("timestats"),
                          height = paste0(panel_par$height, "in")))
    })
    ###############################################################
    # time_list
    shiny::reactiveValues(
      panel       = shiny::reactive("Times"),
      height      = shiny::reactive(panel_par$height),
      postfix     = shiny::reactive({
        shiny::req(time_table())
        filename <- paste(names(time_table()$traits), collapse = ",")
        if(shiny::req(main_par$plot_table) == "Tables")
          filename <- paste0(stringr::str_remove(panel_par$table, " "), "_",
                             filename)
        stringr::str_replace_all(filename, ": ", "_")
      }),
      plotObject  = shiny::reactive({
        print(shiny::req(timeplots()))
        print(shiny::req(timestats()))
      }),
      tableObject = shiny::reactive({
        shiny::req(time_table())
        switch(shiny::req(panel_par$table),
               "Cell Means" = summary_traitTime(time_table()),
               Stats        = stats_time_table(time_table()$stats))
      })
    )
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

