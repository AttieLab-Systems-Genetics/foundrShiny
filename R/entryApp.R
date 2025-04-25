#' Entry App for password
#'
#' @param id identifier for shiny reactive
#' @param customSettings list of custom settings
#' @return reactive server
#' 
#' @importFrom shiny checkboxGroupInput debounce hideTab observeEvent
#'             passwordInput reactive reactiveVal renderUI req showTab
#' @importFrom grDevices dev.off pdf
#' @importFrom utils write.csv
#' @importFrom foundr timetraitsall
#' @export
entryApp <- function() {
  ui <- shiny::bootstrapPage(
    entryInput("entry"),
    entryOutput("entry")
  )
  server <- function(input, output, session) {
    entryServer("entry", customSettings)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname entryApp
entryServer <- function(id, customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Don't show Entry Key if there is no need.
    output$entry <- shiny::renderUI({
      if(shiny::isTruthy(customSettings$entrykey))
        # Debounce password to prevent chatter.
        password_debounce()
    })
    password <- shiny::reactive({
      shiny::passwordInput(ns("entry_key"), "Entry Key:", input$entry_key)
    })
    password_debounce <- shiny::debounce(password, 10000)
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
#' @rdname entryApp
entryInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("entry"))
}
#' @export
#' @rdname entryApp
entryOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("entry_show"))
}
