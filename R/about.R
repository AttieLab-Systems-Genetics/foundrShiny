#' About Server
#'
#' @param id identifier for shiny reactive
#' @param helppath path to help markdown
#'
#' @return reactive server
#' @export
aboutServer <- function(id, helppath = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$about <- shiny::renderUI({
      if(!is.null(helppath) && helppath != "" && file.exists(helppath)) {
        datainfo <- shiny::includeMarkdown(helppath)
      } else {
        datainfo <- shiny::includeMarkdown(
          system.file(file.path("shinyApp", "help.md"), package='foundrShiny'))
      }
      tagList(
        shiny::includeMarkdown(
          system.file(file.path("shinyApp", "intro.md"), package='foundrShiny')),
        "See GitHub:",
        shiny::a(paste("byandell/foundrShiny",
                       paste0("(version ",
                              utils::packageVersion("foundrShiny"), "); ")),
                 href = "https://github.com/byandell/foundrShiny"),
        shiny::a(paste("byandell/foundr",
                       paste0("(version ", utils::packageVersion("foundr"), ")")),
                 href = "https://github.com/byandell/foundr"),
        shiny::br(),
        shiny::br(),
        datainfo)
    })
  })
}
#' @export
#' @rdname aboutServer
aboutOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("about"))
}
#' @export
#' @rdname aboutServer
aboutApp <- function(id) {
  ui <- shiny::bootstrapPage(
    aboutOutput("about")
  )
  server <- function(input, output, session) {
    aboutServer("about", helppath = customSettings$help)
  }
  shiny::shinyApp(ui, server)
}

