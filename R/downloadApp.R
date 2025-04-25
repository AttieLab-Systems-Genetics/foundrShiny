#' Download App
#'
#' @param id identifier for shiny reactive
#' @param prefix static prefix for filename
#' @param main_par input parameters from calling routine
#' @param download_list reactiveValues with  postfix,plotObject,tableObject
#' @return nothing 
#'
#' @importFrom shiny column downloadButton downloadHandler fluidRow
#'             moduleServer NS renderUI req textAreaInput uiOutput
#' @importFrom utils write.csv    
#' @importFrom grDevices dev.off pdf
#' @export
downloadApp <- function(id) {
  ui <- shiny::bootstrapPage(
    shiny::fluidRow(
      shiny::column(6, mainParInput("main_par")), # dataset
      shiny::column(6, mainParUI("main_par"))), # order
    mainParOutput("main_par"), # plot_table, height
    downloadOutput("download")
  )
  server <- function(input, output, session) { 
    # Test sets
    prefix <- "Download"
    download_list <- list(
      panel = shiny::reactive("Download"),
      postfix = shiny::reactive("postfix"),
      height = shiny::reactive(main_par$height),
      plotObject = shiny::reactive(print(plot_null())),
      tableObject = shiny::reactive(matrix(1:12,nrow=3)))
    # Modules
    main_par <- mainParServer("main_par", traitStats)
    downloadServer("download", prefix, main_par, download_list)
  }
  shiny::shinyApp(ui, server)
}
#' @rdname downloadApp
#' @export
downloadServer <- function(id, prefix, main_par, download_list) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$downloads <- shiny::renderUI({
      plot_table <- shiny::req(main_par$plot_table)
      shiny::downloadButton(ns(plot_table), plot_table)
    })
    output$filename <- renderUI({
      filename <- paste0(shiny::req(download_list$panel()), "_",
                         shiny::req(download_list$postfix()))
      shiny::textAreaInput(ns("filename"), "File Prefix:", filename)
    })
    output$Plots <- shiny::downloadHandler(
      filename = function() paste0(shiny::req(input$filename), ".pdf"),
      content = function(file) {
        grDevices::pdf(file, width = 9,
                       height = shiny::req(download_list$height()))
        shiny::req(download_list$plotObject())
        grDevices::dev.off()
      })
    output$Tables <- shiny::downloadHandler(
      filename = function() paste0(shiny::req(input$filename), ".csv"),
      content = function(file) {
        table <- shiny::req(download_list$tableObject())
        utils::write.csv(table, file, row.names = FALSE)
      })
  })
}
#' @rdname downloadApp
#' @export
downloadOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h5("Download:"),
    shiny::fluidRow(
      shiny::column(3, shiny::uiOutput(ns("downloads"))),
      shiny::column(9, shiny::uiOutput(ns("filename")))))
}
