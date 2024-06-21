#' Shiny Module Server for Trait Stats
#'
#' @param id identifier for shiny reactive
#' @param input,output,session standard shiny arguments
#' @param CorTable reactive data frames
#' @param panel_par,main_par reactive inputs from calling modules
#' @param customSettings static list of settings
#'
#' @return reactive object
#' @importFrom shiny h3 isTruthy moduleServer NS plotOutput reactive renderUI
#'             renderPlot req tagList selectInput uiOutput 
#' @importFrom foundr ggplot_bestcor
#' @export
#'
corPlotServer <- function(id, panel_par, main_par, cors_table,
                         customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$shiny_output <- shiny::renderUI({
      shiny::req(main_par$height, cors_plot())
      shiny::plotOutput(ns("cors_plot"), height = paste0(main_par$height, "in"))
    })
    
    cors_plot <- shiny::reactive({
      shiny::req(input$mincor, cors_table())
      foundr::ggplot_bestcor(
        mutate_datasets(cors_table(), customSettings$dataset, undo = TRUE), 
        input$mincor, shiny::isTruthy(input$abscor))
    })
    output$cors_plot <- shiny::renderPlot({
      shiny::req(cors_plot())
      print(cors_plot())
    })
    ##############################################################
    cors_plot
  })
}
#' Shiny Module Output for Trait Correlations
#' @return nothing returned
#' @rdname corPlotServer
#' @export
corPlotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Correlation"),
    shiny::fluidRow( 
      shiny::column(6, shiny::sliderInput(ns("mincor"), "Minimum:", 0, 1, 0.7)),
      shiny::column(6, shiny::checkboxInput(ns("abscor"), "Absolute Correlation?", TRUE))),
    shiny::uiOutput(ns("shiny_output")))
}
#' Shiny Module App for Trait Correlations
#' @return nothing returned
#' @rdname corPlotServer
#' @export
corPlotApp <- function() {
  title <- "Test Shiny Trait Correlation Plot"
  
  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # Key Datasets and Trait.
        shiny::fluidRow(
          shiny::column(3, mainParInput("main_par")),
          shiny::column(3, mainParUI("main_par")),
          shiny::column(6, traitNamesUI("key_trait"))),
        # Related Datasets and Traits.
        shiny::fluidRow(
          shiny::column(6, shiny::uiOutput("reldataset")),
          shiny::column(6, traitNamesUI("rel_traits"))),
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1)
      ),
      shiny::mainPanel(
        mainParOutput("main_par"),
        shiny::textOutput("key_trait"),
        corTableOutput("cors_table"),
        shiny::textOutput("rel_traits"),
        corPlotOutput("cors_plot")
      )
    )
  )
  server <- function(input, output, session) {
    # MODULES
    # Order Traits by Stats.
    main_par <- mainParServer("main_par", traitStats)
    stats_table <- traitOrderServer("stats_table", main_par, traitStats)
    # Key Trait.
    key_trait    <- traitNamesServer("key_trait", main_par, stats_table)
    # Correlation Table.
    cors_table  <- corTableServer("cors_table", input, main_par,
                                  key_trait, traitSignal)
    # Related Traits.
    rel_traits   <- traitNamesServer("rel_traits", main_par, cors_table, TRUE)
    # Correlation Plot
    cors_plot   <- corPlotServer("cors_plot", input, main_par, cors_table)
    
    # I/O FROM MODULE
    output$key_trait <- renderText({
      shiny::req(stats_table())
      foundr::unite_datatraits(stats_table(), key = TRUE)[1]
    })
    output$rel_traits <- renderText(shiny::req(rel_traits()))
    
    # Related Datasets.
    datasets <- unique(traitStats$dataset)
    output$reldataset <- renderUI({
      shiny::selectInput("reldataset", "Related Datasets:",
                         datasets, datasets[1], multiple = TRUE)
    })
  }
  shiny::shinyApp(ui = ui, server = server)
}
