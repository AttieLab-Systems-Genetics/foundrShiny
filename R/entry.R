#' Shiny Server for entry Key
#'
#' @param id identifier for shiny reactive
#' @param customSettings list of custom settings
#'
#' @return reactive server
#' 
#' @export
#' 
#' @importFrom shiny checkboxGroupInput hideTab observeEvent reactive
#'             reactiveVal renderUI req showTab
#' @importFrom grDevices dev.off pdf
#' @importFrom utils write.csv
#' @importFrom foundr timetraitsall
#'
entryServer <- function(id, customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Don't show Entry Key if there is no need.
    output$entrykey <- shiny::renderUI({
      if(shiny::isTruthy(customSettings$entrykey))
        shiny::textInput(ns("appEntry"), "Entry Key:")
    })
    entrykey <- shiny::reactive({
      out <- !shiny::isTruthy(customSettings$entrykey)
      if(!out & shiny::isTruthy(input$appEntry)) {
        out <- (input$appEntry == customSettings$entrykey)
      }
      shiny::isTruthy(out)
    })
    output$entryflow <- shiny::renderUI({
      shiny::h3(entrykey())
    })
    ############################################################
    entrykey
  })
}
#' @export
#' @rdname entryServer
entryInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("entrykey"))
}
#' @export
#' @rdname entryServer
entryOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("entryflow"))
}
#' @param title title of app
#' @export
#' @rdname entryServer
entryApp <- function(title = "") {
  ui <- shiny::bootstrapPage(
    entryInput("entry"),
    entryOutput("entry")
  )
  server <- function(input, output, session) {
    entryServer("entry", customSettings)
  }
  shiny::shinyApp(ui, server)
}
