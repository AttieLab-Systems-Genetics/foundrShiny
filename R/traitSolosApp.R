#' Trait Solos App
#'
#' @param id identifier for shiny reactive
#' @param input,output,session standard shiny arguments
#' @param panel_par,main_par reactive arguments from `server`
#' @param trait_table reactive objects from `server`
#' @return reactive object
#' 
#' @importFrom shiny moduleServer NS observeEvent plotOutput radioButtons reactive 
#'             renderPlot renderUI req tagList uiOutput
#' @importFrom DT renderDataTable dataTableOutput
#' @importFrom foundr ggplot_traitSolos
#' @export
traitSolosApp <- function() {
  title <- "Test Shiny Trait Solos"
  
  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fluidRow(
          shiny::column(3, mainParInput("main_par")), # dataset
          shiny::column(3, mainParUI("main_par")), # order
          shiny::column(6, traitNamesUI("key_trait"))), # key_trait
        traitTableUI("trait_table"), # butresp
        shiny::fluidRow(
          shiny::column(6, mainParOutput1("main_par")), # plot_table
          shiny::column(6, panelParOutput("panel_par"))) # height or table
      ),
      shiny::mainPanel(
        panelParInput("panel_par"), # strains, facet
        traitSolosOutput("solos_plot"),
        traitTableOutput("trait_table")
      )
    )
  )
  server <- function(input, output, session) {
    main_par <- mainParServer("main_par", traitStats)
    panel_par <- panelParServer("panel_par", main_par, traitStats)
    stats_table <- traitOrderServer("stats_table", main_par,
                                    traitStats, customSettings)
    key_trait    <- traitNamesServer("key_trait", main_par, stats_table)
    rel_traits <- shiny::reactive(NULL, label = "rel_traits")
    trait_table <- traitTableServer("trait_table", panel_par,
                                    key_trait, rel_traits, traitData, traitSignal)
    traitSolosServer("solos_plot", panel_par, main_par, trait_table)
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
#' @rdname traitSolosApp
#' @export
traitSolosServer <- function(id, panel_par, main_par, trait_table) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Output: Plots or Data
    output$solos <- shiny::renderPlot({
      shiny::req(solos_plot())
      print(solos_plot())
    })
    output$solos_plot <- shiny::renderUI({
      shiny::req(solos_plot(), panel_par$height)
      shiny::plotOutput(ns("solos"), height = paste0(panel_par$height, "in"))
    })

    # Plot
    solos_plot <- shiny::reactive({
      shiny::req(trait_table())
      foundr::ggplot_traitSolos(
        trait_table(),
        facet_strain = panel_par$facet,
        boxplot = TRUE)
    },
    label = "solos_plot")
    #############################################################
    solos_plot
  })
}
#' @rdname traitSolosApp
#' @export
traitSolosOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Trait Plots"),
    shiny::uiOutput(ns("solos_plot"))
  )
}
