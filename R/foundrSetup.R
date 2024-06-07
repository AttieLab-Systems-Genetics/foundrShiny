#' Setup Data for Foundr App
#'
#' @param data_instance type of data
#' @param data_subset focus instance to selected dataset(s) if not `NULL` 
#' @param custom_settings setup custom if `TRUE`
#'
#' @return invisible
#' @export
foundrSetup <- function(data_instance = c("Liver","Trait"),
                        data_subset = NULL,
                        custom_settings = TRUE) {
  data_instance <- match.arg(data_instance)
  
  assign("data_instance", data_instance, envir = globalenv())
  assign("data_subset", data_subset, envir = globalenv())
  assign("custom_settings", custom_settings, envir = globalenv())
  # Read trait data and set up custom settings.
  source(system.file(file.path("shinyApp", "TraitData.R"),
                     package = "foundrShiny"))
  invisible()
}