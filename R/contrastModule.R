#' Shiny Module Server for Modules of Contrasts
#'
#' @param id identifier for shiny reactive
#' @param panel_par,main_par reactive arguments 
#' @param moduleContrast,traitContast reactive data frames
#' @param traitModule static data frames
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny h3 moduleServer NS reactive renderPlot renderUI req
#'             selectizeInput tagList uiOutput updateSelectizeInput
#' @importFrom stringr str_to_title
#' @export
#'
contrastModuleServer <- function(id, panel_par, main_par,
                                traitModule, moduleContrast, traitContrast,
                                customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # MODULES
    # Contrast Eigen Plots
    contrastPlotServer("contrast_plot",
                      panel_par, main_par, contrastTable, customSettings,
                      modTitle)
    
    contrastTable <- shiny::reactive({
      if(shiny::isTruthy(panel_par$module)) traits() else eigens()      
    })
    modTitle <- shiny::reactive({
      if(shiny::isTruthy(panel_par$module)) 
        paste("Eigentrait Contrasts for Module", panel_par$module)
      else
        "Eigentrait Contrasts across Modules"
    })
    
    # INPUTS
    
    # Restrict `traitModule` to datasets in `moduleContrast()`
    datamodule <- shiny::reactive({
      traitModule[shiny::req(main_par$dataset)]
    })
    
    # Eigen Contrasts.
    eigens <- shiny::reactive({
      shiny::req(datamodule(), moduleContrast())
      
      eigen_contrast_dataset(datamodule(), moduleContrast())
    })
    
    # Compare Selected Module Eigens to Traits in Module
    traits <- shiny::reactive({
      shiny::req(datamodule(), panel_par$sex, panel_par$module, main_par$dataset,
                 traitContrast(), eigens())
      
      eigen_traits_dataset(datamodule(), panel_par$sex, panel_par$module,
                           traitContrast(), eigens())
    })
    
    ##############################################################
    eigens
  })
}
#' Shiny Module Input for Modules of Contrasts
#' @return nothing returned
#' @rdname contrastModuleServer
#' @export
contrastModuleInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    contrastPlotInput(ns("contrast_plot")),
    contrastPlotUI(ns("contrast_plot")))
}
#' Shiny Module Output for Modules of Contrasts
#' @return nothing returned
#' @rdname contrastModuleServer
#' @export
contrastModuleOutput <- function(id) {
  ns <- shiny::NS(id)
  contrastPlotOutput(ns("contrast_plot"))
}
#' Shiny Module App for Modules of Contrasts
#' @return nothing returned
#' @rdname contrastModuleServer
#' @export
contrastModuleApp <- function() {
  title <- "Shiny Module Contrast Module"
  
  ui <- function() {
    
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::fluidRow(
            shiny::column(4, datasetInput("dataset")),
            shiny::column(8, contrastTableInput("contrast_table"))
          )
        ),
        shiny::mainPanel(
          contrastModuleInput("contrast_module"),
          shiny::fluidRow(
            shiny::column(4, shiny::uiOutput("sex")),
            shiny::column(8, shiny::uiOutput("module"))),
          contrastModuleOutput("contrast_module")
        )
      )
    )
  }
  
  server <- function(input, output, session) {
    
    # This now works with two calls to contrast_table.
    # However, it is slow because the second call to create `traitContrast`
    # pulls in all 52965 traits.
    # Would like to only pull in those for selected module.
    # But this requires moving `module` parameter up one level (to panel).
    # This gets tricky because also need to move up `sex`.
    
    # Other consideration: modifying this stuff to do same thing with stats
    # across traits in a module.
    
    # *** need persistent module choice (reactiveVal)
    # *** table from traits()
    # *** sliders from Volcano
    # *** simplify using traitModule as below
    # *** move module choice to side panel
    
    # *** need new handling for Mark's modules
    # *** see `eigen_traits_contr_object` in eigen_traits.R
    # *** and mods to `eigen_traits_dataset_value`
    
    # MODULE
    main_par <- datasetServer("dataset", traitStats)
    # Contrast Module Table
    mods_table <- contrastTableServer("contrast_table", input, main_par,
      traitSignal, traitStats, customSettings)
    # Contrast Trait Table
    trait_table <- contrastTableServer("contrast_table", input, main_par,
      traitSignal, traitStats, customSettings, keepDatatraits)
    # Contrast Modules.
    contrastModuleServer("contrast_module", input, main_par,
      traitModule, mods_table, trait_table)
    
    # SERVER-SIDE INPUTS
    sexes <- c(B = "Both Sexes", F = "Female", M = "Male", C = "Sex Contrast")
    output$sex <- shiny::renderUI({
      shiny::selectInput("sex", "", as.vector(sexes))
    })
    output$module <- shiny::renderUI({
      shiny::selectizeInput("module", "Module:", NULL)
    })
    shiny::observeEvent(
      shiny::req(datatraits(), main_par$dataset, input$sex),
      {
        # First zero out input$module.
        shiny::updateSelectizeInput(session, "module",
                                    selected = "", server = TRUE)
        # Then set choices.
        shiny::updateSelectizeInput(session, "module", choices = datatraits(),
                                    selected = "", server = TRUE)
      })
    
    datamodule <- shiny::reactive({
      traitModule[shiny::req(main_par$dataset[1])]
    })
    datatraits <- shiny::reactive({
      shiny::req(input$sex, main_par$dataset, datamodule())
      if(foundr:::is_sex_module(datamodule())) {
        out <- unique(datamodule()[[main_par$dataset[1]]][[input$sex]]$modules$module)
        paste0(main_par$dataset[1], ": ", names(sexes)[match(input$sex, sexes)], "_", out)
      } else {
        paste0(main_par$dataset[1], ": ", unique(datamodule()[[main_par$dataset]]$value$modules$module))
      }
    }, label = "datatraits")
    
    keepDatatraits <- reactive({
      module <- NULL
      if(shiny::isTruthy(input$module))
        module <- input$module
      
      foundr:::keptDatatraits(traitModule, shiny::req(main_par$dataset)[1], module)
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
