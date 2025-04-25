#' Trait Names App
#'
#' Select trait names in one of two modes, depending on the fixed `multiples`:
#' `FALSE` = only one trait name,
#' `TRUE` =  multiple names.
#' The order of choices depends on `traitArranged()`.
#' 
#' @param id identifier for shiny reactive
#' @param main_par reactive arguments 
#' @param traitArranged reactive data frames
#' @param multiples fixed logical for multiple trait names
#' @return reactive vector of trait names
#' 
#' @importFrom shiny moduleServer NS observeEvent reactive req
#'             selectizeInput uiOutput updateSelectizeInput
#' @importFrom DT renderDataTable
#' @importFrom dplyr distinct
#' @importFrom rlang .data
#' @importFrom foundr unite_datatraits
#' @export
traitNamesApp <- function() {
  title <- "Test Shiny Trait Names"
  ui <- function() {
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::fluidRow(
            shiny::column(6, mainParInput("main_par")), # dataset
            shiny::column(6, mainParUI("main_par"))), # order
          # Key Dataset and Trait.
          traitNamesUI("key_trait"), # key_trait
          # Related Datasets and Traits.
          shiny::fluidRow(
            shiny::column(6, corTableInput("cors_table")), # rel_dataset
            shiny::column(6, traitNamesUI("rel_traits"))) # rel_traits
        ),
        shiny::mainPanel(
        )
      )
    )
  }
  server <- function(input, output, session) {
    main_par <- mainParServer("main_par", traitStats)
    stats_table <- traitOrderServer("stats_table", main_par,
                                    traitStats, customSettings)
    # Key Trait and Correlation Table.
    key_trait   <- traitNamesServer("key_trait", main_par, stats_table)
    cors_table  <- corTableServer("cors_table", main_par,
                                  key_trait, traitSignal, customSettings)
    # Related Traits.
    rel_traits  <- traitNamesServer("rel_traits", main_par, cors_table, TRUE)
  }
  shiny::shinyApp(ui = ui, server = server)
}
#' @rdname traitNamesApp
#' @export
traitNamesServer <- function(id, main_par, traitArranged, multiples = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Select traits
    output$trait_names <- shiny::renderUI({
      inputId <- ifelse(multiples, "Related Traits:", "Key Trait:")
      shiny::selectizeInput(ns("trait_names"), inputId,
        choices = shiny::req(traitNamesArranged()), multiple = multiples)
    })
    shiny::observeEvent(
      shiny::req(main_par$dataset, main_par$order, traitArranged()), {
      choices <- traitNamesArranged()
      selected <- input$trait_names
      if(!shiny::isTruthy(selected)) {
        selected <- NULL
      } else {
        if (!(all(selected %in% choices))) selected <- NULL
      }
      if(!multiples & is.null(selected)) selected <- choices[1]
      shiny::updateSelectizeInput(session, "trait_names", choices = choices,
                                  server = TRUE, selected = selected)
    },
    ignoreNULL = FALSE, label = "update_trait")

    traitNamesArranged <- shiny::reactive({
      if(shiny::isTruthy(traitArranged())) {
        foundr::unite_datatraits(
          dplyr::distinct(
            traitArranged(),
            .data$dataset, .data$trait))
      } else {
        NULL
      }
    },
    label = "traitNamesArranged")
    
    ###############################################
    # vector returned as reactive
    shiny::reactive(input$trait_names)
  })
}
#' @rdname traitNamesApp
#' @export
traitNamesUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("trait_names"))
}
