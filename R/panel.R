#' Shiny panel Server for foundr Package
#'
#' @param id identifier for shiny reactive
#' @param traitData,traitSignal,traitStats,traitModule static objects
#' @param customSettings list of custom settings
#' @return reactive server
#' @export
panelServer <- function(id,
                   traitData = NULL, traitSignal = NULL, traitStats = NULL,
                   customSettings = NULL, traitModule = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    main_par <- mainParServer("main_par", traitStats)
    trait_list <- traitServer("tabTraits", main_par,
      traitData, traitSignal, traitStats, customSettings)
    contrast_list <- contrastServer("tabContrasts", main_par,
      traitSignal, traitStats, traitModule, customSettings)
    stats_list <- statsServer("tabStats", main_par,
      traitStats, customSettings)
    time_list <- timeServer("tabTimes", main_par,
      traitData, traitSignal, traitStats)
    
    # Side Input
    output$sideInput <- shiny::renderUI({
      shiny::req(input$tabpanel)
      
      # Tab-specific side panel.
      shiny::req(input$tabpanel)
      if(input$tabpanel != "About") {
        shiny::tagList(
          shiny::fluidRow(
            shiny::column(6, mainParInput(ns("main_par"))), # dataset
            if(input$tabpanel %in% c("Traits", "Times"))
              shiny::column(6, mainParUI(ns("main_par"))), # order
          ),
          if(input$tabpanel %in% c("Traits","Times","Contrasts")) {
            has_time_data <- length(foundr::timetraitsall(traitSignal) > 0)
            switch(input$tabpanel, # key_trait and 
                   Traits    = traitInput(ns("tabTraits")), # rel_dataset, rel_traits
                   Contrasts = contrastInput(ns("tabContrasts")), # time_unit
                   Times     = if(has_time_data)
                     timeInput(ns("tabTimes"))) # time_unit, response
          }
        )
      }
    })
    # Main Output
    output$mainOutput <- shiny::renderUI({
      shiny::tabsetPanel(
        type = "tabs", header = "", id = ns("tabpanel"),
        shiny::tabPanel("Traits",    traitOutput(ns("tabTraits"))),
        shiny::tabPanel("Contrasts", contrastOutput(ns("tabContrasts"))),
        shiny::tabPanel("Stats",     statsOutput(ns("tabStats"))),
        shiny::tabPanel("Times",     timeOutput(ns("tabTimes")))
      )
    })
  })
}
#' @export
#' @rdname panelServer
panelInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("sideInput")),
    shiny::uiOutput(ns("entrykey")))
}
#' @export
#' @rdname panelServer
panelOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("mainOutput"))
}
#' @param title title of app
#' @export
#' @rdname panelServer
panelApp <- function(title = "") {
  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        panelInput("panel")
      ),
      shiny::mainPanel(
        panelOutput("panel")
      )
    )
  )
  server <- function(input, output, session) {
    panelServer("panel",
                 traitData, traitSignal, traitStats,
                 customSettings, traitModule)
  }
  shiny::shinyApp(ui, server)
}
