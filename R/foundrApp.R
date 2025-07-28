#' foundr App for foundr package
#'
#' @param id identifier for shiny reactive
#' @param traitData,traitSignal,traitStats,traitModule static objects
#' @param customSettings list of custom settings
#' 
#' @importFrom shiny fluidPage mainPanel moduleServer NS reactive renderUI req
#'             sidebarLayout sidebarPanel uiOutput
#' @export
foundrApp <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Foundr App"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        foundrInput("foundr")
      ),
      shiny::mainPanel(
        foundrOutput("foundr")
      )
    )
  )
  server <- function(input, output, session) {
    foundrServer("foundr",
                 traitData, traitSignal, traitStats,
                 customSettings, traitModule)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname foundrApp
foundrServer <- function(id,
                   traitData = NULL, traitSignal = NULL, traitStats = NULL,
                   customSettings = NULL, traitModule = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # CALL MODULES
    entry <- entryServer("entry", customSettings)
    panelServer("panel",
                traitData, traitSignal, traitStats,
                customSettings, traitModule, entry)
    output$entry <- shiny::renderUI({
      if(entry() == 1) {
        list(
          entryInput(ns("entry")),
          entryUI(ns("entry")),
          entryOutput(ns("entry"))
        )
      }
    })
    
    # Side Input
    output$sideInput <- shiny::renderUI({
      list(
        shiny::uiOutput(ns("entry")),
        panelInput(ns("panel")))
    })
    # Main Output
    output$mainOutput <- shiny::renderUI({
      panelOutput(ns("panel"))
    })
  })
}
#' @export
#' @rdname foundrApp
foundrInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("sideInput"))
}
#' @export
#' @rdname foundrApp
foundrOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("mainOutput"))
}
