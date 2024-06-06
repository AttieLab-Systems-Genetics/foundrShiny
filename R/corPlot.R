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
corPlotServer <- function(id, panel_par, main_par, CorTable,
                         customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # calling module inputs
    #   input$mincor:     Minimum Correlation
    #   main_par$height:    Plot height
    # shinyCorPlot inputs
    #   input$abscor:         Absolute Correlation
    #
    # RETURNS
    # cors_plot()
    
    output$shiny_output <- shiny::renderUI({
      shiny::req(main_par$height, cors_plot())
      
      shiny::plotOutput(ns("cors_plot"),
                        height = paste0(main_par$height, "in"))
    })
    
    cors_plot <- shiny::reactive({
      shiny::req(input$mincor, CorTable())
      foundr::ggplot_bestcor(
        mutate_datasets(CorTable(), customSettings$dataset, undo = TRUE), 
        input$mincor, shiny::isTruthy(input$abscor))
    })
    output$cors_plot <- shiny::renderPlot({
      shiny::req(cors_plot())
      
      print(cors_plot())
    })
    
    ##############################################################
    # Return
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
  # Read trait data.
  source(system.file(file.path("shinyApp", "TraitData.R"), package = "foundrShiny"))
  
  title <- "Test Shiny Trait Correlation Plot"
  
  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # Key Datasets and Trait.
        shiny::fluidRow(
          shiny::column(3, shiny::uiOutput("dataset")),
          shiny::column(3, traitOrderInput("shinyOrder")),
          shiny::column(6, traitNamesUI("shinyKeyTrait"))),
        # Related Datasets and Traits.
        shiny::fluidRow(
          shiny::column(6, shiny::uiOutput("reldataset")),
          shiny::column(6, traitNamesUI("shinyNames"))),
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1)
      ),
      shiny::mainPanel(
        shiny::textOutput("key_trait"),
        corTableOutput("shinyCorTable"),
        shiny::textOutput("rel_traits"),
        corPlotOutput("shinyCorPlot")
      )
    )
  )
  server <- function(input, output, session) {
    # MODULES
    # Order Traits by Stats.
    stats_table <- traitOrderServer("shinyOrder", input, input, traitStats)
    # Key Trait.
    keyTrait    <- traitNamesServer("shinyKeyTrait", input, stats_table)
    # Correlation Table.
    cors_table  <- corTableServer("shinyCorTable", input, input,
                                  keyTrait, traitSignal)
    # Related Traits.
    relTraits   <- traitNamesServer("shinyNames", input, cors_table, TRUE)
    # Correlation Plot
    cors_plot   <- corPlotServer("shinyCorPlot", input, input, cors_table)
    
    # I/O FROM MODULE
    output$dataset <- shiny::renderUI({
      # Dataset selection.
      datasets <- unique(traitStats$dataset)
      # Get datasets.
      shiny::selectInput("dataset", "Datasets:",
                         datasets, datasets[1], multiple = TRUE)
    })
    output$key_trait <- renderText({
      shiny::req(stats_table(), cors_table())
      foundr::unite_datatraits(cors_table(), key = TRUE)[1]
    })
    output$rel_traits <- renderText(shiny::req(relTraits()))
    
    # Related Datasets.
    datasets <- shiny::reactive(unique(traitStats$dataset))
    output$reldataset <- renderUI({
      shiny::req(datasets())
      shiny::selectInput("reldataset", "Related Datasets:",
                         datasets(), datasets()[1], multiple = TRUE)
    })
  }
  shiny::shinyApp(ui = ui, server = server)
}
