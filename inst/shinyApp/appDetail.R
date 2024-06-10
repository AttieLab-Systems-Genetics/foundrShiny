devtools::install_cran("plotly") #  not yet on UW dataviz
devtools::install_cran("markdown") #  not yet on UW dataviz
devtools::install_cran("cowplot") #  not yet on UW dataviz
devtools::install_cran("ggdendro") #  not yet on UW dataviz
devtools::install_github("byandell/foundr")
devtools::install_github("byandell/foundrShiny")
options(shiny.sanitize.errors = FALSE)

#foundrShiny::foundrSetup(data_instance = "Liver",
#                         data_subset = c("Physio","MixMod"),
#                         custom_settings = TRUE)

dirpath <- "~/data"
data_instance <- "liver"

traitData   <- readRDS(file.path(dirpath, paste0(data_instance, "Data.rds")))
traitSignal <- readRDS(file.path(dirpath, paste0(data_instance, "Signal.rds")))
traitStats  <- readRDS(file.path(dirpath, paste0(data_instance, "Stats.rds")))
traitModule <- readRDS(file.path(dirpath, "traitModule.rds"))

foundr::link_datasets(traitSignal, file.path(dirpath, "source.csv"),
                      file.path(dirpath))
datasets <- readRDS(file.path(dirpath, "datasets.rds"))

customSettings <- list(
  help = file.path(dirpath, "help.md"),
  condition = "diet",
  entrykey = "Founder",
  dataset = datasets)

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
