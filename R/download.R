#' Shiny Module Server for Downloads
#'
#' @param id identifier for shiny reactive
#' @param prefix static prefix for filename
#' @param main_par input parameters from calling routine
#' @param download_list reactiveValues with  postfix,plotObject,tableObject
#'
#' @return nothing 
#' @importFrom shiny column downloadButton downloadHandler fluidRow
#'             moduleServer NS renderUI req textAreaInput uiOutput
#' @importFrom utils write.csv    
#' @importFrom grDevices dev.off pdf
#' @export
#'
downloadServer <- function(id, prefix, main_par, download_list) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # DOWNLOADS
    output$downloads <- shiny::renderUI({
      shiny::req(main_par$butshow)
      
      shiny::downloadButton(ns(paste0("download", main_par$butshow)),
                            main_par$butshow)
    })
    # Download File Prefix
    output$filename <- renderUI({
      filename <- paste0(prefix, "_", shiny::req(download_list$postfix()))
      
      shiny::textAreaInput(ns("filename"), "File Prefix:", filename)
    })
    
    # Download Plot
    output$downloadPlots <- shiny::downloadHandler(
      filename = function() paste0(shiny::req(input$filename), ".pdf"),
      content = function(file) {
        grDevices::pdf(file, width = 9, height = main_par$height)
        shiny::req(download_list$plotObject())
        grDevices::dev.off()
      })
    
    # Download DataTable
    output$downloadTables <- shiny::downloadHandler(
      filename = function() paste0(shiny::req(input$filename), ".csv"),
      content = function(file) {
        utils::write.csv(shiny::req(download_list$tableObject()),
                         file, row.names = FALSE)
      })
  })
}
#' Shiny Module Output for Downloads
#' @return nothing returned
#' @rdname downloadServer
#' @export
downloadOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::renderText("DownloadsOutput"),
    shiny::fluidRow(
      shiny::column(3, shiny::uiOutput(ns("downloads"))),
      shiny::column(9, shiny::uiOutput(ns("filename")))))
}
#' Shiny Module App for Downloads
#' @return nothing returned
#' @rdname downloadServer
#' @export
downloadApp <- function(id) {
  ui <- shiny::bootstrapPage(
    mainParInput("main_par"),
    downloadOutput("download"),
    mainParOutput("main_par")
  )
  server <- function(input, output, session) { 
    # Test sets
    prefix <- "Download"
    download_list <- list(
      postfix = shiny::reactive("postfix"),
      plotObject = shiny::reactive(print(foundr:::plot_null())),
      tableObject = shiny::reactive(matrix(1:12,nrow=3)))
    
    # Modules
    main_par <- mainParServer("main_par", traitStats)
    downloadServer("download", prefix, main_par, download_list)
  }
  shiny::shinyApp(ui, server)
}
