#' Shiny Module Server for Correlation Table
#'
#' @param id identifier for shiny reactive
#' @param panel_par,main_par reactive inputs from calling modules
#' @param key_trait reactive character string
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
corTableServer <- function(id, panel_par, main_par,
                          key_trait, traitSignal,
                          customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$cors_table <- DT::renderDataTable(
      {
        shiny::req(key_trait(), cors_table())
        foundr::summary_bestcor(
          mutate_datasets(cors_table(), customSettings$dataset),
          0.0)
      },
      escape = FALSE, options = list(scrollX = TRUE, pageLength = 5))
    
    cors_table <- shiny::reactive({
      if(!shiny::isTruthy(key_trait())) return(NULL)
      cor_table(key_trait(), traitSignal, "cellmean", 0.0,
                panel_par$reldataset)
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
          shiny::column(3, mainParInput("main_par")), # dataset
          shiny::column(3, mainParUI("main_par")), # order
          shiny::column(6, traitNamesUI("key_trait"))), # key_trait
        # Related Datasets and Traits.
        shiny::uiOutput("reldataset")),
      
      shiny::mainPanel(
        shiny::textOutput("orderTable"),
        shiny::textOutput("key_trait"),
        shiny::textOutput("cors_table"),
        traitOrderUI ("stats_table"),
        corTableOutput("cors_table")
      )
    )
  )
  server <- function(input, output, session) {
    main_par <- mainParServer("main_par", traitStats)
    stats_table <- traitOrderServer("stats_table", main_par, traitStats)
    key_trait    <- traitNamesServer("key_trait", main_par, stats_table)
    cors_table  <- corTableServer("cors_table", input, main_par, key_trait, traitSignal)
    
    # I/O FROM MODULE
    output$key_trait <- shiny::renderText(paste("key_trait", shiny::req(key_trait())))
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
