#' Shiny Module Server for trait solos Plots
#'
#' @param id identifier for shiny reactive
#' @param input,output,session standard shiny arguments
#' @param panel_par,main_par reactive arguments from `foundrServer`
#' @param trait_names reactive with trait names
#' @param traitSolosObject reactive objects from `foundrServer`
#'
#' @return reactive object for `traitSolos`
#' 
#' @importFrom shiny isTruthy moduleServer observeEvent NS plotOutput radioButtons 
#'             reactive renderPlot renderUI req tagList uiOutput
#' @importFrom DT renderDataTable dataTableOutput
#' @importFrom foundr ggplot_traitPairs traitPairs
#' @export
#'
traitPairsServer <- function(id, panel_par, main_par, trait_names, traitSolosObject) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # Main inputs:
    #   main_par$height
    #   panel_par$facet
    # TraitPairs inputs:
    #   input$pair (obsolete)
    
    # OUTPUTS
    # output$pairs_plot
    
    # RETURNS
    # pairs_plot()
    
    # Output: Plots or Data
    output$shiny_traitPairs <- shiny::renderUI({
      shiny::req(trait_names(), traitSolosObject())
      shiny::plotOutput(ns("pairs_plot"), height = paste0(main_par$height, "in"))
    })
    
    # Plot
    pairs_plot <- shiny::reactive({
      shiny::req(traitSolosObject(), trait_names())
      
      foundr::ggplot_traitPairs(
        foundr::traitPairs(
          traitSolosObject(),
          trait_names(),
          pair()),
        facet_strain = shiny::isTruthy(panel_par$facet),
        parallel_lines = TRUE)
    },
    label = "pairs_plot")
    output$pairs_plot <- shiny::renderPlot({
      print(pairs_plot())
    })
    
    # INPUT PAIR
    pair <- shiny::reactive({
      trait_pairs(trait_names())
    },
    label = "pair")
    # Obsolete
    output$pair <- shiny::renderUI({
      # Somehow when main_par$height is changed this is reset.
      shiny::req(trait_names())
      if(length(trait_names()) < 2)
        return(NULL)
      choices <- trait_pairs(trait_names(), key = FALSE)
      
      shiny::selectInput(
        "pair", "Select pairs for scatterplots",
        choices = choices, selected = choices[1],
        multiple = TRUE, width = '100%')
    })
    
    #############################################################
    
    pairs_plot
  })
}
#' Shiny Module UI for TraitPairs
#' @return nothing returned
#' @rdname traitPairsServer
#' @export
traitPairsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("shiny_traitPairs"))
}
#' Shiny Module App for TraitPairs
#' @return nothing returned
#' @rdname traitPairsServer
#' @export
traitPairsApp <- function(id) {
  # Read trait data.
  source(system.file(file.path("shinyApp", "TraitData.R"), package = "foundrShiny"))
  
  title <- "Test Shiny Trait Pairs"
  
  ui <- function() {
    # INPUTS
    #   input$facet: Facet by strain?
    #   input$strains: Strains to select
    #   input$height: Plot Height
    #   input$
    #
    # OUTPUTS (see shinyTraitSolos)
    #   output$filename: 
    #   output$downloadPlot
    #   output$downloadTable
    
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::uiOutput("traits"),
          shiny::uiOutput("reltraits"),
          traitTableUI("shinyObject"),
          shiny::uiOutput("strains"), # See SERVER-SIDE INPUTS below
          shiny::checkboxInput("facet", "Facet by strain?", FALSE),
          shiny::sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1),
          shiny::fluidRow(
            shiny::column(6, shiny::uiOutput("filename")), # See MODULE INPUT below
            shiny::column(3, shiny::downloadButton("downloadPlot", "Plots")),
            shiny::column(3, shiny::downloadButton("downloadTable", "Data")))
        ),
        
        shiny::mainPanel(
          traitPairsUI("shinyPairs"),
          traitTableOutput("shinyObject")
        )))
  }
  
  server <- function(input, output, session) {
    
    # SHINY MODULES
    trait_table <- traitTableServer("shinyObject", input, input, keyTrait, relTraits, traitData, traitSignal)
    pairs_plot <- traitPairsServer("shinyPairs", input, input, trait_names, trait_table)
    
    # SERVER-SIDE INPUTS
    output$strains <- shiny::renderUI({
      choices <- names(foundr::CCcolors)
      shiny::checkboxGroupInput("strains", "Strains",
        choices = choices, selected = choices, inline = TRUE)
    })
    output$traits <- shiny::renderUI({
      traits <- unique(foundr::unite_datatraits(traitSignal))[1:5]
      
      shiny::selectInput("trait", "Trait:", traits)
    })
    output$reltraits <- shiny::renderUI({
      traits <- unique(foundr::unite_datatraits(traitSignal))[6:10]
      
      shiny::selectInput("reltrait", "Related Trait:", traits)
    })
    # Mockup of trait names
    keyTrait <- shiny::reactive(shiny::req(input$trait), label = "keyTrait")
    relTraits <- shiny::reactive(shiny::req(input$reltrait), label = "relTraits")
    trait_names <- shiny::reactive({
      c(shiny::req(keyTrait()), relTraits())
    },
    label = "trait_names")
    
    
    # RETURN OBJECTS FROM MODULES
    datasets <- shiny::reactive({
      shiny::req(trait_table())
      unique(trait_table()$dataset)
    })
    
    # I/O FROM MODULE
    
    # MODULE INPUT: File Prefix
    output$filename <- renderUI({
      shiny::req(datasets())
      filename <- paste0("module_", paste(datasets(), collapse = "."))
      shiny::textAreaInput("filename", "File Prefix", filename)
    })
    
    # MODULE OUTPUT: Plot
    output$downloadPlot <- shiny::downloadHandler(
      filename = function() {
        paste0(shiny::req(input$filename), ".pdf")
      },
      content = function(file) {
        grDevices::pdf(file, width = 9, height = 6)
        print(pairs_plot())
        grDevices::dev.off()
      })
    
    # MODULE OUTPUT: DataTable
    output$downloadTable <- shiny::downloadHandler(
      filename = function() {
        paste0(shiny::req(input$filename), ".csv")
      },
      content = function(file) {
        utils::write.csv(
          trait_table(),
          file, row.names = FALSE)
      })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
