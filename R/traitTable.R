#' Shiny Module Server for Trait Table
#'
#' @param id identifier for shiny reactive
#' @param input,output,session standard shiny arguments
#' @param panel_par reactive arguments
#' @param key_trait,rel_traits reactives with trait names
#' @param traitData,traitSignal static objects 
#'
#' @return reactive object for `shinyTrait` routines
#' 
#' @importFrom shiny h3 moduleServer NS radioButtons reactive reactiveVal
#'             renderUI req tagList uiOutput
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom foundr subset_trait_names traitSolos unite_datatraits
#' @importFrom utils write.csv
#' @export
#'
traitTableServer <- function(id, panel_par, key_trait, rel_traits,
                            traitData, traitSignal,
                            customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # Main inputs:
    #   panel_par$strains
    #   panel_par$reldataset
    # traitObject inputs: (see traitObjectUI)
    #   input$butresp
    
    # RETURNS
    # trait_table()
    
    # Wrap input$butresp
    resp_selection <- shiny::reactiveVal(NULL, label = "resp_selection")
    shiny::observeEvent(input$butresp,
                        resp_selection(input$butresp))
    
    # Filter static traitData based on selected trait_names.
    keyData <- shiny::reactive({
      shiny::req(key_trait())
      
      foundr::subset_trait_names(traitData, key_trait())
    })

    # trait_table Data Frame
    trait_table <- shiny::reactive({
      foundr::traitSolos(
        traitData,
        traitSignal,
        shiny::req(trait_names()),
        shiny::req(resp_selection()),
        shiny::req(panel_par$strains))
    }, label = "trait_table")
    
    trait_names <- shiny::reactive({
      c(shiny::req(key_trait()), rel_traits())
    }, label = "trait_names")
    
    # Data Table
    datameans <- shiny::reactive({
      shiny::req(trait_table())
      
      summary(trait_table(), customSettings)
    })
    
    output$shiny_traitObject <- DT::renderDataTable(
      shiny::req(datameans()),
      escape = FALSE,
      options = list(scrollX = TRUE, pageLength = 10))
    
    #############################################################
    trait_table
  })
}
#' Shiny Module UI for Trait Table
#' @return nothing returned
#' @rdname traitTableServer
#' @export
traitTableUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::radioButtons(ns("butresp"), "Response",
                      c("value", "normed", "cellmean"),
                      "value", inline = TRUE)
}
#' Shiny Module UI for Trait Table
#' @return nothing returned
#' @rdname traitTableServer
#' @export
traitTableOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Cell Means"),
    DT::dataTableOutput(ns("shiny_traitObject")))
}
#' Shiny Module App for Trait Table
#' @return nothing returned
#' @rdname traitTableServer
#' @export
traitTableApp <- function() {
  title <- "Test Shiny Trait Table"
  
  ui <- function() {
    # INPUTS
    #   input$facet: Facet by strain?
    #   input$strains: Strains to select
    #   input$height: Plot Height
    #   input$
    #
    # OUTPUTS (see shinyTraitTable)
    #   trait_table()
    
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          mainParInput("main_par"),
          shiny::uiOutput("traits"),
          shiny::uiOutput("strains"), # See SERVER-SIDE INPUTS below
          traitTableUI("shinyTest")
        ),
        
        shiny::mainPanel(
          traitTableOutput("shinyTest")
        )))
  }
  
  server <- function(input, output, session) {
    
    # MODULES
    main_par <- mainParServer("main_par", traitSignal)
    trait_table <- traitTableServer("shinyTest", input,
      key_trait, rel_traits, traitData, traitSignal)
    # Mockup of trait names
    key_trait <- shiny::reactive(shiny::req(input$trait), label = "key_trait")
    rel_traits <- shiny::reactive(NULL, label = "rel_traits")
    
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
      shiny::selectInput("trait","Traits:", traits)
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}