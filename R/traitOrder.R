#' Shiny Module Server for Trait Order
#'
#' 
#' @param id identifier for shiny reactive
#' @param panel_par,main_par input reactive list
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
traitOrderServer <- function(id, panel_par, main_par,
                            traitStats,
                            customSettings = NULL, keepDatatraits = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    #   main_par$tabpanel
    # shinyTraitOrder inputs
    #   input$keydataset
    #   input$order
    #
    # RETURNS
    #   stats_table()
    
    # Key Datasets.
    #    output$keydataset <- renderUI({
    #      datasets <- unique(traitStats$dataset)
    #      choices <- datasets[1]
    #      if(allDatasets) choices <- datasets
    #      shiny::selectInput(ns("keydataset"), "Key Datasets:",
    #                         datasets, choices, multiple = TRUE)
    #    })
    key_selection <- shiny::reactiveVal(NULL, label = "key_selection")
    shiny::observeEvent(main_par$dataset, key_selection(main_par$dataset))

    # Order Criteria for Trait Names
    output$order <- shiny::renderUI({
      choices <- order_choices(traitStats)
      shiny::selectInput(ns("order"), "Order traits by", choices)
    })
    order_selection <- shiny::reactiveVal(NULL, label = "order_selection")
    shiny::observeEvent(input$order, order_selection(input$order))
    
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
      shiny::req(order_selection(), key_stats())
      
      order_trait_stats(order_selection(), key_stats())
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
        if(shiny::isTruthy(key_selection())) {
          dplyr::filter(
            traitStats,
            .data$dataset %in% key_selection())
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
#' Shiny Module Input for Trait Order
#' @rdname traitOrderServer
#' @export
traitOrderInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("order"))
}
#' Shiny Module UI for Trait Order
#' @rdname traitOrderServer
#' @export
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
          shiny::uiOutput("dataset"),
          traitOrderInput("shinyOrder"),
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
    stats_table <- traitOrderServer("shinyOrder", input, input, traitStats)
    
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