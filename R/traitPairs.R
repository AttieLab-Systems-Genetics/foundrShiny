#' Shiny Module Server for trait solos Plots
#'
#' @param id identifier for shiny reactive
#' @param input,output,session standard shiny arguments
#' @param panel_par,main_par reactive arguments from `foundrServer`
#' @param trait_names reactive with trait names
#' @param trait_table reactive objects from `foundrServer`
#'
#' @return reactive object for `traitSolos`
#' 
#' @importFrom shiny isTruthy moduleServer observeEvent NS plotOutput radioButtons 
#'             reactive renderPlot renderUI req tagList uiOutput
#' @importFrom DT renderDataTable dataTableOutput
#' @importFrom foundr ggplot_traitPairs traitPairs
#' @importFrom dplyr distinct filter
#' @importFrom rlang .data
#' @export
#'
traitPairsServer <- function(id, panel_par, main_par, trait_names,
                             trait_table) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # Main inputs:
    #   main_par$height

    # OUTPUTS
    # output$pairs_plot
    
    # RETURNS
    # pairs_plot()
    
    # Output: Plots or Data
    output$shiny_traitPairs <- shiny::renderUI({
      shiny::req(trait_names(), trait_table())
      shiny::plotOutput(ns("pairs_plot"), height = paste0(main_par$height, "in"))
    })
    
    # Plot
    pairs_plot <- shiny::reactive({
      shiny::req(trait_table(), trait_names())
      
      foundr::ggplot_traitPairs(
        foundr::traitPairs(
          trait_table(),
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
  title <- "Test Shiny Trait Pairs"
  ui <- function() {
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::fluidRow(
            shiny::column(6, mainParInput("main_par")), # dataset
            shiny::column(6, shiny::uiOutput("traits"))),
          shiny::uiOutput("rel_traits"),
          traitTableUI("trait_table"),
          shiny::uiOutput("strains"), # See SERVER-SIDE INPUTS below
          shiny::checkboxInput("facet", "Facet by strain?", FALSE)
        ),
        
        shiny::mainPanel(
          mainParOutput("main_par"), # plot_table, height
          shiny::uiOutput("plottable")
        )))
  }
  
  server <- function(input, output, session) {
    
    # SHINY MODULES
    main_par <- mainParServer("main_par", traitStats)
    trait_table <- traitTableServer("trait_table", input,
      key_trait, rel_traits, traitData, traitSignal)
    traitPairsServer("pairs_plot", input, main_par, trait_names, trait_table)
    
    # SERVER-SIDE INPUTS
    output$strains <- shiny::renderUI({
      choices <- names(foundr::CCcolors)
      shiny::checkboxGroupInput("strains", "Strains",
        choices = choices, selected = choices, inline = TRUE)
    })
    output$traits <- shiny::renderUI({
      traits <- foundr::unite_datatraits(
        dplyr::distinct(
          dplyr::filter(traitSignal, .data$dataset %in% main_par$dataset),
          .data$dataset, .data$trait))
      
      shiny::selectInput("trait", "Trait:", traits)
    })
    output$rel_traits <- shiny::renderUI({
      traits <- unique(foundr::unite_datatraits(traitSignal))[6:10]
      
      shiny::selectInput("rel_traits", "Related Trait:", traits)
    })
    # Mockup of trait names
    key_trait <- shiny::reactive(shiny::req(input$trait), label = "key_trait")
    rel_traits <- shiny::reactive(shiny::req(input$rel_traits),
                                  label = "rel_traits")
    trait_names <- shiny::reactive({
      c(shiny::req(key_trait()), rel_traits())
    },
    label = "trait_names")
    
    # Outputs
    output$plottable <- shiny::renderUI({
      switch(main_par$plot_table,
        Plots = shiny::tagList(
          shiny::h3("Solos Plot"),
          traitPairsUI("pairs_plot")),
        Tables = traitTableOutput("trait_table"))
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
