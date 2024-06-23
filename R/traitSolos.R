#' Shiny Module Server for trait solos Plots
#'
#' @param id identifier for shiny reactive
#' @param input,output,session standard shiny arguments
#' @param panel_par,main_par reactive arguments from `server`
#' @param trait_table reactive objects from `server`
#'
#' @return reactive object
#' 
#' @importFrom shiny moduleServer NS observeEvent plotOutput radioButtons reactive 
#'             renderPlot renderUI req tagList uiOutput
#' @importFrom DT renderDataTable dataTableOutput
#' @importFrom foundr ggplot_traitSolos
#' @export
#'
traitSolosServer <- function(id, panel_par, main_par, trait_table) {
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
    output$solos_plot <- shiny::renderPlot({
      shiny::req(solos_plot(), main_par$height)
      shiny::plotOutput(print(solos_plot()), height = paste0(main_par$height, "in"))
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
#' Shiny Module UI for traitSolos
#' @return nothing returned
#' @rdname traitSolosServer
#' @export
traitSolosUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("solos_plot"))
}
#' Shiny Module AI for traitSolos
#' @return nothing returned
#' @rdname traitSolosServer
#' @export
traitSolosApp <- function() {
  title <- "Test Shiny Trait Solos"
  
  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fluidRow(
          shiny::column(3, mainParInput("main_par")), # dataset
          shiny::column(9, shiny::uiOutput("traits"))),
        traitTableUI("trait_table"),
        shiny::uiOutput("strains"), # See SERVER-SIDE INPUTS below
        shiny::checkboxInput("facet", "Facet by strain?", FALSE)
      ),
      
      shiny::mainPanel(
        mainParOutput("main_par"), # plot_table, height
        shiny::uiOutput("plottable")
      )
    )
  )
  
  server <- function(input, output, session) {
    # MODULES
    main_par <- mainParServer("main_par", traitStats)
    trait_table <- traitTableServer("trait_table", input,
      key_trait, rel_traits, traitData, traitSignal)
    traitSolosServer("solos_plot", input, main_par, trait_table)
    
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
      
      shiny::selectInput("traits", "Trait:", traits)
    })
    
    output$plottable <- shiny::renderUI({
      switch(main_par$plot_table,
        Plots = shiny::tagList(
          shiny::h3("Solos Plot"),
          traitSolosUI("solos_plot")),
        Tables = traitTableOutput("trait_table"))
    })
    
    # REACTIVES
    key_trait <- shiny::reactive({
      shiny::req(input$traits)[1]
    })
    rel_traits <- shiny::reactive(NULL)
  }
  
  shiny::shinyApp(ui = ui, server = server)
}