#' Entry App for password
#'
#' @param id identifier for shiny reactive
#' @param customSettings list of custom settings
#' @return reactive server
#' 
#' @importFrom shiny checkboxGroupInput checkboxInput debounce hideTab h4
#'             isTruthy observeEvent passwordInput reactive renderUI
#'             req showTab sliderInput
#' @importFrom grDevices dev.off pdf
#' @importFrom utils write.csv
#' @importFrom foundr timetraitsall
#' @export
entryApp <- function() {
  ui <- shiny::bootstrapPage(
    entryInput("entry"),
    entryUI("entry"),
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
    # Debounce password input to prevent chatter.
    output$entry_key <- shiny::renderUI({
      if(shiny::isTruthy(customSettings$entrykey)) password_debounce()
    })
    password <- shiny::reactive({
      entry_key <- input$entry_key
#      if(shiny::isTruthy(entry_val())) entry_key <- entry_val()
#      message("get entry_key ", entry_key, " input ",
#              input$entry_key, " val ", entry_val())
      if(!entry()) {
        shiny::passwordInput(ns("entry_key"), "Entry Key:", entry_key)
      }
    })
#    entry_val <- shiny::reactiveVal("")
#    shiny::observeEvent(input$entry_key, {
#      message("observe entry_key ", input$entry_key, " val ", entry_val())
#      if(shiny::isTruthy(input$entry_key)) {
#        entry_val(input$entry_key)
#      }
#    })
    debounce <- shiny::reactive({
      if(shiny::isTruthy(input$debounce)) {
        10 ^ shiny::req(input$debounce)
      } else {
        10^5
      }
    })
    password_debounce <- shiny::debounce(password, debounce)
    entry <- shiny::reactive({
      out <- !shiny::isTruthy(customSettings$entrykey)
      if(!out & shiny::isTruthy(input$entry_key)) {
        out <- (input$entry_key == customSettings$entrykey)
      }
      shiny::isTruthy(out)
    })
    output$entry_show <- shiny::renderUI({
      if(shiny::isTruthy(input$reveal)) {
        list(
          shiny::h4(paste("Typed:", input$entry_key)),
          shiny::h4(paste("Logic: ", entry())))
      }
    })
    output$debounce <- shiny::renderUI({
      debounce <- 5
      if(shiny::isTruthy(input$debounce)) debounce <- input$debounce
      shiny::sliderInput(ns("debounce"),"Debounce", 3, 10,
                         debounce, 1)
    })
    output$reveal <- shiny::renderUI({
      reveal <- shiny::isTruthy(input$reveal)
      shiny::checkboxInput(ns("reveal"), "Show password?", reveal)
    })
    ############################################################
    entry
  })
}
#' @export
#' @rdname entryApp
entryInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("entry_key"))
}
#' @export
#' @rdname entryApp
entryUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("debounce"))
}
#' @export
#' @rdname entryApp
entryOutput <- function(id) {
  ns <- shiny::NS(id)
  list(
    shiny::uiOutput(ns("reveal")),
    shiny::uiOutput(ns("entry_show")))
}
