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
    
    panel_par <- panelParServer("panel_par", main_par, traitStats)
    time_table <- timeTableServer("time_table", panel_par, main_par, 
      traitData, traitSignal, traitStats)
    time_plot <- timePlotServer("time_plot", panel_par, main_par,
      traitSignal, time_table)
    
    output$plot_table <- shiny::renderUI({
      shiny::tagList(
        panelParInput(ns("panel_par")), # strains, facet
        if(shiny::req(main_par$plot_table) == "Tables") {
          shiny::radioButtons(ns("plot_table"), "Download:",
            c("Cell Means","Stats"), "Cell Means", inline = TRUE)
        },
        switch(shiny::req(main_par$plot_table),
               Plots  = timePlotOutput(ns("time_plot")),
               Tables = timeTableOutput(ns("time_table")))
      )
    })
    
    ###############################################################
    shiny::reactiveValues(
      postfix = shiny::reactive({
        shiny::req(time_table())
        filename <- paste(names(time_table()$traits), collapse = ",")
        if(shiny::req(main_par$plot_table) == "Tables")
          filename <- paste0(stringr::str_remove(input$buttable, " "), "_",
                             filename)
        stringr::str_replace_all(filename, ": ", "_")
      }),
      plotObject = shiny::reactive({
        shiny::req(time_plot())
        print(time_plot()$Traits)
        print(time_plot()$Stats)
      }),
      tableObject = shiny::reactive({
        shiny::req(time_table())
        switch(shiny::req(input$buttable),
               "Cell Means" = summary_traitTime(time_table()),
               Stats        = stats_time_table(time_table()$stats))
      })
    )
  })
}
#' Shiny Module Input for Time Panel
#' @return nothing returned
#' @rdname timeServer
#' @export
timeInput <- function(id) { # Traits
  ns <- shiny::NS(id)
  timeTableInput(ns("time_table")) # traits
}
#' Shiny Module UI for Time Panel
#' @return nothing returned
#' @rdname timeServer
#' @export
timeUI <- function(id) { # Time Unit
  ns <- shiny::NS(id)
  timeTableUI(ns("time_table")) # time_units, response
}
#' Shiny Module Output for Times Plot
#' @return nothing returned
#' @rdname timeServer
#' @export
timeOutput <- function(id) { # Response; Plots or Tables
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("plot_table")))
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
            shiny::column(3, mainParInput("main_par")), # dataset
            shiny::column(3, mainParUI("main_par")), # order
            shiny::column(6, timeInput("time"))), # traits
          timeUI("time"), # time_units, response
          border_line(),
          mainParOutput("main_par"), # plot_table, height
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

