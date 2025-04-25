#' About Server
#'
#' @param id identifier for shiny reactive
#' @param helppath path to help markdown
#' @param entry entry logical flag
#'
#' @return reactive server
#' @export
#' @importFrom shiny a bootstrapPage br includeMarkdown isTruthy NS reactive
#'             renderUI shinyApp strong uiOutput
aboutApp <- function(id) {
  ui <- shiny::bootstrapPage(
    aboutOutput("about")
  )
  server <- function(input, output, session) {
    aboutServer("about", helppath = customSettings$help)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname aboutApp
aboutServer <- function(id, helppath = NULL,
                        entry = shiny::reactive( TRUE )) {
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
        if(shiny::isTruthy(entry())) {
          shiny::strong("Click on tabs above this line to see data panels.")
        },
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
#' @rdname aboutApp
aboutOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("about"))
}
