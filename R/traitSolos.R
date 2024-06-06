#' Shiny Module Server for trait solos Plots
#'
#' @param id identifier for shiny reactive
#' @param input,output,session standard shiny arguments
#' @param panel_par,main_par reactive arguments from `server`
#' @param traitSolosObject reactive objects from `server`
#'
#' @return reactive object
#' 
#' @importFrom shiny moduleServer NS observeEvent plotOutput radioButtons reactive 
#'             renderPlot renderUI req tagList uiOutput
#' @importFrom DT renderDataTable dataTableOutput
#' @importFrom foundr ggplot_traitSolos
#' @export
#'
traitSolosServer <- function(id, panel_par, main_par, traitSolosObject) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # Main inputs:
    #   panel_par$facet
    #   main_par$height
    
    # OUTPUTS
    # output$shiny_solosPlot
    
    # RETURNS
    # solos_plot()
    
    #############################################################
    # Output: Plots or Data
    output$shiny_solosPlot <- shiny::renderUI({
      shiny::req(main_par$height)
      
      shiny::plotOutput(ns("solos_plot"), height = paste0(main_par$height, "in"))
    })
    
    # Plot
    solos_plot <- shiny::reactive({
      shiny::req(traitSolosObject())
      
      foundr::ggplot_traitSolos(
        traitSolosObject(),
        facet_strain = panel_par$facet,
        boxplot = TRUE)
    },
    label = "solos_plot")
    output$solos_plot <- shiny::renderPlot({
      shiny::req(solos_plot())
      
      print(solos_plot())
    })
    
    #############################################################
    
    solos_plot
  })
}
#' Shiny Module UI for traitSolos
#' @return nothing returned
#' @rdname traitSolosServer
#' @export
traitSolosUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("shiny_solosPlot"))
}
#' Shiny Module AI for traitSolos
#' @return nothing returned
#' @rdname traitSolosServer
#' @export
traitSolosApp <- function() {
  # Read trait data.
  source(system.file(file.path("shinyApp", "TraitData.R"), package = "foundrShiny"))
  
  title <- "Test Shiny Trait Solos"
  
  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::column(3, shiny::uiOutput("dataset")),
        shiny::selectInput("trait","Traits:",c("Enrich: 15N2-Urea_enrichment_120_18wk","Enrich: N-Methyl-D3-Creatinine_enrichment_0_18wk","Enrich: 5,5,5-D3-Leucine_enrichment_120_18wk","Enrich: Trimethyl-D9-Carnitine_enrichment_60_18wk")),
        traitTableUI("shinyObject"),
        shiny::uiOutput("strains"), # See SERVER-SIDE INPUTS below
        shiny::checkboxInput("facet", "Facet by strain?", FALSE),
        shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1),
        shiny::fluidRow(
          shiny::column(6, shiny::uiOutput("filename")), # See MODULE INPUT below
          shiny::column(3, shiny::downloadButton("downloadPlot", "Plots")),
          shiny::column(3, shiny::downloadButton("downloadTable", "Data"))
        )
      ),
      
      shiny::mainPanel(
        traitSolosUI("shinySolos"),
        traitTableOutput("shinyObject")
      )
    )
  )
  
  server <- function(input, output, session) {
    # MODULES
    trait_table <- traitTableServer("shinyObject", input, input,
      keyTrait, relTraits, traitData, traitSignal)
    traitSolosServer("shinySolos", input, input, trait_table)
    
    # SERVER-SIDE INPUTS
    output$dataset <- shiny::renderUI({
      # Dataset selection.
      datasets <- unique(traitStats$dataset)
      
      # Get datasets.
      shiny::selectInput("dataset", "Datasets:",
                         datasets, datasets[1], multiple = TRUE)
    })
    output$strains <- shiny::renderUI({
      choices <- names(foundr::CCcolors)
      shiny::checkboxGroupInput("strains", "Strains",
                                choices = choices, selected = choices, inline = TRUE)
    })
    
    # REACTIVES
    keyTrait <- shiny::reactive(shiny::req(input$trait)[1])
    relTraits <- shiny::reactive(NULL)
    datasets <- shiny::reactive({
      shiny::req(trait_table())
      unique(trait_table()$dataset)
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}