#' Shiny Module Server for Trait Order
#'
#' 
#' @param id identifier for shiny reactive
#' @param main_par input reactive list
#' @param traitStats static data frame
#' @param customSettings custom settings list
#' @param keepDatatraits keep datatraits if not `NULL`
#'
#' @return reactive object
#' @importFrom shiny column fluidRow h3 moduleServer NS observeEvent reactive 
#'             reactiveVal renderUI req selectInput shinyApp tagList uiOutput
#'             updateSelectInput
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom plotly plotlyOutput renderPlotly
#' @importFrom foundr summary_strainstats
#' @export
#'
traitOrderServer <- function(id, main_par,
  traitStats, customSettings = NULL, keepDatatraits = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Table
    output$key_stats <- DT::renderDataTable(
      {
        shiny::req(stats_table())
        
        # Summary gives nice table; use open thresholds to include all.
        foundr::summary_strainstats(stats_table(), threshold = c(deviance = 0, p = 1))
      },
      escape = FALSE,
      options = list(scrollX = TRUE, pageLength = 5))
    
    stats_table <- shiny::reactive({
      shiny::req(main_par$order, key_stats())
      order_trait_stats(main_par$order, key_stats())
    })
    key_stats <- shiny::reactive({
      if(shiny::isTruthy(keepDatatraits())) {
        dplyr::select(
          dplyr::filter(
            tidyr::unite(
              traitStats,
              datatraits, dataset, trait, sep = ": ", remove = FALSE),
            datatraits %in% keepDatatraits()),
          -datatraits)
      } else {
        if(shiny::isTruthy(main_par$dataset)) {
          dplyr::filter(
            traitStats,
            .data$dataset %in% main_par$dataset)
        } else {
          NULL
        }
      }
    })
    
    ##########################################################
    # Return
    stats_table
  })
}
traitOrderUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(  
    shiny::h3("Stats"),
    DT::dataTableOutput(ns("key_stats")))
}
#' Shiny Module App for Trait Order
#' @rdname traitOrderServer
#' @export
traitOrderApp <- function() {
  title <- "Test Shiny Trait Order Table"
  
  ui <- function() {
    
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          # Key Datasets and Trait.
          mainParInput("main_par"),
          # Related Datasets and Traits.
          shiny::uiOutput("reldataset")),
        
        shiny::mainPanel(
          shiny::tagList(
            shiny::textOutput("key_trait"),
            traitOrderUI("shinyOrder"))
        )))
  }
  
  server <- function(input, output, session) {
    
    # INPUTS
    # OUTPUTS
    #   output$key_trait: Key Trait
    # OUTPUTS
    #   stats_table()
    
    # MODULES
    # Order Traits by Stats.
    main_par <- mainParServer("main_par", traitStats)
    stats_table <- traitOrderServer("shinyOrder", main_par, traitStats)
    
    # I/O FROM MODULE
    output$dataset <- shiny::renderUI({
      # Dataset selection.
      datasets <- unique(traitStats$dataset)
      
      # Get datasets.
      shiny::selectInput("dataset", "Datasets:",
                         datasets, datasets[1], multiple = TRUE)
    })
    output$key_trait <- renderText({
      shiny::req(stats_table())
      
      foundr::unite_datatraits(stats_table(), key = TRUE)[1]
    })
    
  }
  
  shiny::shinyApp(ui = ui, server = server)
}