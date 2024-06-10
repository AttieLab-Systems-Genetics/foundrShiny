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
    output$shiny_solosPlot <- shiny::renderUI({
      shiny::req(main_par$height)
      
      shiny::plotOutput(ns("solos_plot"), height = paste0(main_par$height, "in"))
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
  title <- "Test Shiny Trait Solos"
  
  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fluidRow(
          shiny::column(3, datasetInput("dataset")),
          shiny::column(9, shiny::uiOutput("traits"))),
        traitTableUI("trait_table"),
        shiny::uiOutput("strains"), # See SERVER-SIDE INPUTS below
        shiny::checkboxInput("facet", "Facet by strain?", FALSE),
        datasetUI("dataset"),
        shiny::fluidRow(
          shiny::column(6, shiny::uiOutput("filename")), # See MODULE INPUT below
          shiny::column(3, shiny::downloadButton("downloadPlot", "Plots")),
          shiny::column(3, shiny::downloadButton("downloadTable", "Data"))
        )
      ),
      
      shiny::mainPanel(
        traitSolosUI("solos_plot"),
        traitTableOutput("trait_table")
      )
    )
  )
  
  server <- function(input, output, session) {
    # MODULES
    main_par <- datasetServer("dataset", traitSignal)
    trait_table <- traitTableServer("trait_table", input,
      keyTrait, relTraits, traitData, traitSignal)
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
    
    # REACTIVES
    keyTrait <- shiny::reactive({
      shiny::req(input$traits)[1]
      })
    relTraits <- shiny::reactive(NULL)
  }
  
  shiny::shinyApp(ui = ui, server = server)
}