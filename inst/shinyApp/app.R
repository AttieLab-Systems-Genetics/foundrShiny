devtools::install_cran("plotly") #  not yet on UW dataviz
devtools::install_cran("markdown") #  not yet on UW dataviz
devtools::install_cran("cowplot") #  not yet on UW dataviz
devtools::install_cran("ggdendro") #  not yet on UW dataviz
devtools::install_github("byandell/foundr", ref = "foundrBase")
devtools::install_github("byandell/foundrShiny")
options(shiny.sanitize.errors = FALSE)

foundrShiny::foundrSetup(data_instance = "Liver",
                         #data_subset = c("Physio","MixMod"),
                         custom_settings = TRUE,
                         dirpath = "~/Documents/Research/attie_alan/FounderDietStudy/deployLiver/")

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
server <- function(input, output, session) {
  foundrShiny::foundrServer("foundr",
               traitData, traitSignal, traitStats,
               customSettings, traitModule)
}
shiny::shinyApp(ui, server)
