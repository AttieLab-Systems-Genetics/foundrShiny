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

    # Identify all Time Traits.
    timetrait_all <- foundr::timetraitsall(traitSignal)
    
    keyTrait <- shiny::reactive({
      shiny::req(traitTimesData())
      
      timetraits_filter(traitSignal, traitTimesData())
    })
    relTraits <- shiny::reactiveVal(NULL)
    
    output$plotfront <- shiny::renderUI({
      if(shiny::req(main_par$plot_table) == "Tables") {
        shiny::radioButtons(ns("buttable"), "Download:",
                            c("Cell Means","Stats"), "Cell Means", inline = TRUE)
      }
    })
    output$plotstables <- shiny::renderUI({
      switch(shiny::req(main_par$plot_table),
             Plots  = shiny::uiOutput(ns("plots")),
             Tables = shiny::uiOutput(ns("tables")))
    })
    
    # Tables.
    statstable <- shiny::reactive({
      shiny::req(traitTimesData())
      stats_time_table(traitTimesData()$stats)
    }, label = "statstable")
    traitstable <- shiny::reactive({
      shiny::req(traitTimesData())
      summary_traitTime(traitTimesData())
    }, label = "statstable")
    output$tables <- shiny::renderUI({
      shiny::req(statstable())
      
      shiny::tagList(
        shiny::h3("Cell Means"),
        DT::renderDataTable(traitstable(), escape = FALSE,
                            options = list(scrollX = TRUE, pageLength = 10)),
        
        shiny::h3("Stats: p.value"),
        DT::renderDataTable(statstable(), escape = FALSE,
                            options = list(scrollX = TRUE, pageLength = 10)))
    })
    
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
    shiny::reactiveValues(
      postfix = shiny::reactive({
        filename <- paste(names(traitTimesData()$traits), collapse = ",")
        if(shiny::req(main_par$plot_table) == "Tables")
          filename <- paste0(stringr::str_remove(input$buttable, " "), "_",
                             filename)
        stringr::str_replace_all(filename, ": ", "_")
      }),
      plotObject = shiny::reactive({
        print(shiny::req(timeplots()))
        print(shiny::req(timestats()))
      }),
      tableObject = shiny::reactive({
        shiny::req(traitTimesData())
        switch(shiny::req(input$buttable),
               "Cell Means" = traitstable(),
               Stats        = statstable())
      })
    )
  })
}
#' Shiny Module UI for Time Plots
#' @return nothing returned
#' @rdname timePlotServer
#' @export
timePlotInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("plotfront"))
}
#' Shiny Module Output for Time Plots
#' @return nothing returned
#' @rdname timePlotServer
#' @export
timePlotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("plotstables")) # Plots and Tables
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
            shiny::column(6, mainParInput("main_par")), # dataset
            shiny::column(6, timeInput("time"))),
          timeUI("time")
        ),
        
        shiny::mainPanel(
          mainParOutput("main_par"), # plot_table, height
          timeOutput("time")
        )))
  }
  
  server <- function(input, output, session) {
    # MODULES
    main_par <- mainParServer("main_par", traitStats)
    time_table <- timeTableServer("shinyTimeTable", input, main_par, 
                                  traitData, traitSignal, traitStats)
    time_list <- timePlotServer("shinyTimePlot", input, main_par,
                                traitSignal, time_table)
  }
  
  shiny::shinyApp(ui = ui, server = server)
}

