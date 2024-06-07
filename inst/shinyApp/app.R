foundrShiny::foundrSetup(data_instance = "Liver",
                         data_subset = NULL,
                         custom_settings = TRUE,
                         install_packages = TRUE)

title <- "Founder Diet Study"

ui <- shiny::fluidPage(
  shiny::titlePanel(title),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      foundrShiny::foundrInput("foundr")
    ),
    shiny::mainPanel(
      foundrShiny::foundrOutput("foundr")
    )
  )
)
server <- function(input, output, server) {
  foundrShiny::foundrServer("foundr",
               traitData, traitSignal, traitStats,
               customSettings, traitModule)
}
shiny::shinyApp(ui, server)
