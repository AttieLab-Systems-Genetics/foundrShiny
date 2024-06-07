devtools::install_cran("plotly") #  not yet on UW dataviz
devtools::install_cran("markdown") #  not yet on UW dataviz
devtools::install_cran("cowplot") #  not yet on UW dataviz
devtools::install_cran("ggdendro") #  not yet on UW dataviz
devtools::install_github("byandell/foundr")
devtools::install_github("byandell/foundrShiny")
options(shiny.sanitize.errors = FALSE)

foundrShiny::foundrSetup(data_instance = "Liver",
                         data_subset = NULL,
                         custom_settings = TRUE)

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
