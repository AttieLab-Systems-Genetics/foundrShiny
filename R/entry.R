#' Shiny Server for entry Key
#'
#' @param id identifier for shiny reactive
#' @param customSettings list of custom settings
#'
#' @return reactive server
#' 
#' @export
#' 
#' @importFrom shiny checkboxGroupInput hideTab observeEvent passwordInput
#'             reactive reactiveVal renderUI req showTab
#' @importFrom grDevices dev.off pdf
#' @importFrom utils write.csv
#' @importFrom foundr timetraitsall
#'
entryServer <- function(id, customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Don't show Entry Key if there is no need.
    output$entry <- shiny::renderUI({
      if(shiny::isTruthy(customSettings$entrykey))
        shiny::passwordInput(ns("entry_key"), "Entry Key:", input$entry_key)
    })
    entry <- shiny::reactive({
      out <- !shiny::isTruthy(customSettings$entrykey)
      if(!out & shiny::isTruthy(input$entry_key)) {
        out <- (input$entry_key == customSettings$entrykey)
      }
      shiny::isTruthy(out)
    })
    output$entry_show <- shiny::renderUI({
      shiny::h3(entry())
    })
    ############################################################
    entry
  })
}
#' @export
#' @rdname entryServer
entryInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("entry"))
}
#' @export
#' @rdname entryServer
entryOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("entry_show"))
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
