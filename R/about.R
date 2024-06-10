about <- function(helppath = NULL) {
  if(!is.null(helppath) && helppath != "" && file.exists(helppath)) {
    datainfo <- shiny::includeMarkdown(helppath)
  } else {
    datainfo <- shiny::includeMarkdown(
      system.file(file.path("shinyApp", "help.md"), package='foundrShiny'))
  }
  
  renderUI({
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
}
