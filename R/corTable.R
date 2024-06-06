#' Shiny Module Server for Correlation Table
#'
#' @param id identifier for shiny reactive
#' @param main_par,traits_par reactive inputs from calling modules
#' @param keyTrait reactive character string
#' @param traitSignal static data frame
#' @param customSettings list of custom settings
#'
#' @return reactive object
#' @importFrom dplyr distinct filter select
#' @importFrom tidyr unite
#' @importFrom shiny h3 isTruthy moduleServer NS observeEvent reactive reactiveVal
#'             renderText renderUI req selectInput shinyApp tagList
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom rlang .data
#' @importFrom foundr bestcor summary_bestcor
#' @export
#'
corTableServer <- function(id, main_par, traits_par,
                          keyTrait, traitSignal,
                          customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # calling module inputs
    #   traits_par$reldataset: Related Datasets
    #
    # RETURNS
    # cors_table()
    
    output$cors_table <- DT::renderDataTable(
      {
        shiny::req(keyTrait(), cors_table())
        
        foundr::summary_bestcor(
          mutate_datasets(
            cors_table(),
            customSettings$dataset),
          0.0)
      },
      escape = FALSE,
      options = list(scrollX = TRUE, pageLength = 5))
    
    cors_table <- shiny::reactive({
      if(!shiny::isTruthy(keyTrait()))
        return(NULL)
      
      cor_table(keyTrait(), traitSignal,
               "cellmean", 0.0,
               traits_par$reldataset)
    })
    
    ##############################################################
    # Return
    cors_table
  })
}
#' Shiny Module UI for Trait Correlations
#' @return nothing returned
#' @rdname corTableServer
#' @export
corTableOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Correlation"),
    DT::dataTableOutput(ns("cors_table")))
}
#' Shiny Module App for Trait Correlations
#' @return nothing returned
#' @rdname corTableServer
#' @export
corTableApp <- function() {
  title <- "Test Shiny Trait Correlation Table"
  
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
        shiny::uiOutput("reldataset")),
      
      shiny::mainPanel(
        shiny::textOutput("orderTable"),
        shiny::textOutput("keyTrait"),
        shiny::textOutput("cors_table"),
        traitOrderUI ("shinyOrder"),
        corTableOutput("shinyCorTable")
      )
    )
  )
  server <- function(input, output, session) {
    # INPUTS (see shinyTraitStats)
    # OUTPUTS (see shinyTraitStats)
    #   output$key_trait: Key Trait
    #   output$key_stats: Key Dataset Stats
    #   output$rel_traits: Related Traits
    #   output$corstable: Table of Datasets Correlations
    #   output$corsplot: Plot of Datasets Correlations
    # OUTPUTS (see shinyTraitStats)
    #   cors_table()
    
    # MODULES
    # Order Traits by Stats.
    stats_table <- traitOrderServer("shinyOrder", input, input, traitStats)
    # Key Trait.
    keyTrait    <- traitNamesServer("shinyKeyTrait", input, stats_table)
    # Correlation Table.
    cors_table  <- corTableServer("shinyCorTable", input, input, keyTrait, traitSignal)
    
    # I/O FROM MODULE
    output$dataset <- shiny::renderUI({
      # Dataset selection.
      datasets <- unique(traitStats$dataset)
      # Get datasets.
      shiny::selectInput("dataset", "Datasets:",
                         datasets, datasets[1], multiple = TRUE)
    })
    output$keyTrait <- shiny::renderText(paste("keyTrait", shiny::req(keyTrait())))
    output$orderTable <- renderText({
      shiny::req(stats_table())
      paste("stats_table", foundr::unite_datatraits(stats_table())[1])
    })
    output$cors_table <- shiny::renderText({
      shiny::req(cors_table())
      paste("cors_table", foundr::unite_datatraits(cors_table(), key = TRUE)[1])
    })
    
    # Related Datasets.
    datasets <- shiny::reactive(unique(traitStats$dataset))
    output$reldataset <- shiny::renderUI({
      shiny::req(datasets())
      shiny::selectInput("reldataset", "Related Datasets:",
                         datasets(), datasets()[1], multiple = TRUE)
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
