#' Shiny Module Server for Trait Names
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
#'
#' @return reactive vector of trait names
#' 
#' @importFrom shiny moduleServer NS observeEvent reactive req
#'             selectizeInput uiOutput updateSelectizeInput
#' @importFrom DT renderDataTable
#' @importFrom dplyr distinct
#' @importFrom rlang .data
#' @importFrom foundr unite_datatraits
#' @export
#'
traitNamesServer <- function(id, main_par, traitArranged, multiples = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Select traits
    output$shiny_names <- shiny::renderUI({
      inputId <- ifelse(multiples, "Related Traits:", "Key Trait:")
      shiny::selectizeInput(ns("trait"), inputId, choices = NULL,
                            multiple = multiples)
    })
    shiny::observeEvent(
      shiny::tagList(traitArranged(), main_par$tabpanel),
      {
        choices <- traitNamesArranged()
        selected <- trait_selection()
        if(!is.null(selected) && !(all(selected %in% choices)))
          selected <- NULL
        shiny::updateSelectizeInput(session, "trait", choices = choices,
                                    server = TRUE, selected = selected)
      },
      ignoreNULL = FALSE, label = "update_trait")
    trait_selection <- shiny::reactiveVal(NULL, label = "trait_selection")
    shiny::observeEvent(input$trait, trait_selection(input$trait),
                        ignoreNULL = !multiples)
    shiny::observeEvent(traitArranged(), trait_selection(NULL),
                        ignoreNULL = FALSE)
    
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
    trait_selection
  })
}
#' Shiny Module UI for Trait Names
#' @return nothing returned
#' @rdname traitNamesServer
#' @export
traitNamesUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("shiny_names"))
}
#' Shiny App for Trait Names
#' @return nothing returned
#' @rdname traitNamesServer
#' @export
traitNamesApp <- function() {
  title <- "Test Shiny Trait Names"
  
  ui <- function() {
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          mainParInput("main_par"), # dataset
          traitNamesUI("trait_names"),
          traitNamesUI("trait_names2")
        ),
        shiny::mainPanel(
          shiny::uiOutput("name"),
          shiny::uiOutput("name2")
        )
      )
    )
  }
  server <- function(input, output, session) {
    main_par <- mainParServer("main_par", traitStats)
    trait_names <- traitNamesServer("trait_names", main_par,
      traitStatsInput)
    trait_names2 <- traitNamesServer("trait_names2", main_par,
                                    traitStatsInput, TRUE)
    
    # DATA OBJECTS 
    traitStatsInput <- shiny::reactive({
      if(shiny::isTruthy(main_par$dataset)) {
        dplyr::filter(
          traitStats,
          .data$dataset %in% main_par$dataset)
      } else {
        NULL
      }
    },
    label = "traitStatsInput")
    
    # I/O FROM MODULE
    output$name <- renderUI({
      shiny::req(trait_names())
      name <- paste(trait_names(), collapse = ", ")
      shiny::textAreaInput("name", "Trait:", name)
    })
    output$name2 <- renderUI({
      shiny::req(trait_names())
      name <- paste(trait_names2(), collapse = ", ")
      shiny::textAreaInput("name2", "Multiple Traits:", name)
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
