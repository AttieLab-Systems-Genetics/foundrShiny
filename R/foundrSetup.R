#' Setup Data for Foundr App
#'
#' @param instance type of data
#' @param custom setup custom if `TRUE`
#'
#' @return invisible
#' @export
foundrSetup <- function(instance = c("Liver","Trait"), custom = FALSE) {
  instance <- match.arg(instance)
  # Read trait data and set up custom settings.
  cat(paste("Loading", instance, "Data\n"))
  source(system.file(file.path("shinyApp", paste0(instance, "Data.R")),
                     package = "foundrShiny"), echo = TRUE)
  if(custom) {
    cat(paste("Loading", instance, "Custom Settings\n"))
    source(system.file(file.path("shinyApp", paste0(instance, "Custom.R")),
                       package = "foundrShiny"), echo = TRUE)
  }
  invisible()
}