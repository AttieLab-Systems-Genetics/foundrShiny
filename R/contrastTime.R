#' Shiny Module Server for Contrasts over Time
#'
#' @param id identifier for shiny reactive
#' @param panel_par,main_par reactive arguments 
#' @param traitSignal,traitStats static data frames
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny h3 isTruthy moduleServer NS reactive renderText renderUI
#'             tagList
#' @importFrom stringr str_to_title
#' @export
#'
contrastTimeServer <- function(id, panel_par, main_par,
                              traitSignal, traitStats, contrastTable,
                              customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # RETURNS
    #   contrastOutput
    
    # MODULES
    # Identify Time Traits.
    times_list <- timeTraitsServer("times_list", panel_par, main_par,
                                        traitSignal, contrastTable)
    
    # Contrast Time Signal
    shiny::reactive({
      shiny::req(contrastTable(), times_list$traits, panel_par$strains,
                 times_list$response, times_list$time)
      
      # Convert `contrastTable()` to a `Signal` style data frame.
      contrastSignal <- contrast_signal(contrastTable())
      
      traitTimes(contrastSignal, contrastSignal, traitStats,
                 times_list$traits, times_list$time,
                 times_list$response, strains = panel_par$strains)
    }, label = "contrastTime")
  })
}
#' Shiny Module Input for Contrasts over Time
#' @return nothing returned
#' @rdname contrastTimeServer
#' @export
contrastTimeInput <- function(id) {
  ns <- shiny::NS(id)
  timeTraitsInput(ns("times_list"))
}
#' Shiny Module UI for Contrasts over Time
#' @return nothing returned
#' @rdname contrastTimeServer
#' @export
contrastTimeUI <- function(id) {
  ns <- shiny::NS(id)
  timeTraitsUI(ns("times_list"))
}
