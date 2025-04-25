#' Correlation Plot App
#'
#' @param id identifier for shiny reactive
#' @param input,output,session standard shiny arguments
#' @param CorTable reactive data frames
#' @param panel_par reactive inputs from calling modules
#' @param customSettings static list of settings
#' @return reactive object
#'
#' @importFrom shiny h3 isTruthy moduleServer NS plotOutput reactive renderUI
#'             renderPlot req tagList selectInput uiOutput 
#' @importFrom foundr ggplot_bestcor
#' @export
corPlotApp <- function() {
  title <- "Test Shiny Trait Correlation Plot"
  
  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # Key Datasets and Trait.
        shiny::fluidRow(
          shiny::column(3, mainParInput("main_par")), # dataset
          shiny::column(3, mainParUI("main_par")), # order
          shiny::column(6, traitNamesUI("key_trait"))), # key_trait
        # Related Datasets and Traits.
        shiny::fluidRow(
          shiny::column(6, shiny::uiOutput("rel_dataset")), # rel_dataset
          shiny::column(6, traitNamesUI("rel_traits"))), # rel_traits
        mainParOutput("main_par") # plot_table, height
      ),
      shiny::mainPanel(
        shiny::textOutput("key_trait"),
        corTableOutput("cors_table"),
        shiny::textOutput("rel_traits"),
        corPlotOutput("cors_plot")
      )
    )
  )
  server <- function(input, output, session) {
    main_par <- mainParServer("main_par", traitStats)
    stats_table <- traitOrderServer("stats_table", main_par, traitStats)
    key_trait    <- traitNamesServer("key_trait", main_par, stats_table)
    cors_table  <- corTableServer("cors_table", input, main_par,
                                  key_trait, traitSignal)
    rel_traits   <- traitNamesServer("rel_traits", main_par, cors_table, TRUE)
    cors_plot   <- corPlotServer("cors_plot", main_par, cors_table)
    
    # I/O FROM MODULE
    output$key_trait <- renderText({
      shiny::req(stats_table())
      foundr::unite_datatraits(stats_table(), key = TRUE)[1]
    })
    output$rel_traits <- renderText(shiny::req(rel_traits()))
    
    # Related Datasets.
    datasets <- unique(traitStats$dataset)
    output$rel_dataset <- renderUI({
      shiny::selectInput("rel_dataset", "Related Datasets:",
                         datasets, datasets[1], multiple = TRUE)
    })
  }
  shiny::shinyApp(ui = ui, server = server)
}
#' @rdname corPlotApp
#' @export
corPlotServer <- function(id, panel_par, cors_table,
                         customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    cors_plot <- shiny::reactive({
      shiny::req(input$mincor, cors_table())
      foundr::ggplot_bestcor(
        mutate_datasets(cors_table(), customSettings$dataset, undo = TRUE), 
        input$mincor, shiny::isTruthy(input$abscor))
    })
    output$plot <- shiny::renderPlot({
      print(shiny::req(cors_plot()))
    })
    output$cors_plot <- shiny::renderUI({
      shiny::req(cors_plot())
      height <- panel_par$height
      if(is.null(height)) height <- 6
      shiny::plotOutput(ns("plot"), height = paste0(height, "in"))
    })
    ##############################################################
    cors_plot
  })
}
#' @rdname corPlotApp
#' @export
corPlotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Correlation"),
    shiny::fluidRow( 
      shiny::column(6, shiny::sliderInput(ns("mincor"), "Minimum:", 0, 1, 0.7)),
      shiny::column(6, shiny::checkboxInput(ns("abscor"),
        "Absolute Correlation?", TRUE))),
    shiny::uiOutput(ns("cors_plot")))
}
