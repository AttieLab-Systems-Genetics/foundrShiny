#' Panel App
#'
#' @param id identifier for shiny reactive
#' @param traitData,traitSignal,traitStats,traitModule static objects
#' @param customSettings list of custom settings
#' @return reactive server
#' 
#' @importFrom shiny column fluidRow mainPanel moduleServer NS
#'             renderUI req shinyApp sidebarLayout sidebarPanel tabPanel
#'             tabsetPanel tagList titlePanel uiOutput
#' @importFrom foundr timetraitsall
#' @export
panelApp <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Panel App"),
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
#' @export
#' @rdname panelApp
panelServer <- function(id,
                   traitData = NULL, traitSignal = NULL, traitStats = NULL,
                   customSettings = NULL, traitModule = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    entry <- entryServer("entry", customSettings)
    aboutServer("about", customSettings$help, entry)
    main_par <- mainParServer("main_par", traitStats)
    trait_list <- traitServer("trait", main_par,
      traitData, traitSignal, traitStats, customSettings)
    contrast_list <- contrastServer("contrast", main_par,
      traitSignal, traitStats, traitModule, customSettings)
    stats_list <- statsServer("stats", main_par,
      traitStats, customSettings)
    time_list <- timeServer("time", main_par,
      traitData, traitSignal, traitStats)
    
    # Side Input
    output$sideInput <- shiny::renderUI({
      shiny::req(input$tabpanel)
      
      # Tab-specific side panel.
      shiny::req(input$tabpanel)
      if(input$tabpanel == "About") {
        shiny::tagList(
          entryInput(ns("entry")),
          entryUI(ns("entry")),
          entryOutput(ns("entry"))
        )
      } else {
        shiny::tagList(
          shiny::fluidRow(
            shiny::column(6, mainParInput(ns("main_par"))), # dataset
            if(input$tabpanel %in% c("Traits", "Times"))
              shiny::column(6, mainParUI(ns("main_par"))), # order
          ),
          if(input$tabpanel %in% c("Traits","Times","Contrasts")) {
            has_time_data <- length(foundr::timetraitsall(traitSignal) > 0)
            switch(input$tabpanel, # key_trait and 
                   Traits    = traitInput(ns("trait")), # rel_dataset, rel_traits
                   Contrasts = contrastInput(ns("contrast")), # time_unit
                   Times     = if(has_time_data)
                     timeInput(ns("time"))) # time_unit, response
          }
        )
      }
    })
    # Main Output
    output$mainOutput <- shiny::renderUI({
      shiny::tabsetPanel(
        type = "tabs", header = "", id = ns("tabpanel"),
        shiny::tabPanel("About",     aboutOutput(ns("about"))),
        shiny::tabPanel("Traits",    traitOutput(ns("trait"))),
        shiny::tabPanel("Contrasts", contrastOutput(ns("contrast"))),
        shiny::tabPanel("Stats",     statsOutput(ns("stats"))),
        shiny::tabPanel("Times",     timeOutput(ns("time")))
      )
    })
  })
}
#' @export
#' @rdname panelApp
panelInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("sideInput"))
}
#' @export
#' @rdname panelApp
panelOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("mainOutput"))
}
