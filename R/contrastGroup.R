#' Shiny Module Server for Groups of Contrasts
#'
#' @param id identifier for shiny reactive
#' @param panel_par,main_par reactive arguments 
#' @param trait_table,traitContast reactive data frames
#' @param traitModule static data frames
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny h3 moduleServer NS reactive renderPlot renderUI req
#'             selectizeInput tagList uiOutput updateSelectizeInput
#' @importFrom stringr str_to_title
#' @export
#'
contrastGroupServer <- function(id, panel_par, main_par,
                                traitModule, trait_table, group_table,
                                customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    groupname <- stringr::str_to_title(customSettings$group)
    if(!length(groupname)) groupname <- "Group"
    
    # MODULES
    # Contrast Eigen Plots
    contrast_list <- contrastPlotServer("contrast_plot",
                      panel_par, main_par, contrastTable, customSettings,
                      modTitle)
    
    contrastTable <- shiny::reactive({
      if(shiny::isTruthy(panel_par$group)) traits() else eigens()      
    })
    modTitle <- shiny::reactive({
      if(shiny::isTruthy(panel_par$group)) 
        paste("Eigentrait Contrasts for", groupname, panel_par$group)
      else
        paste0("Eigentrait Contrasts across ", groupname, "s")
    })
    
    # INPUTS
    
    # Restrict `traitModule` to datasets in `trait_table()`
    datagroup <- shiny::reactive({
      traitModule[shiny::req(main_par$dataset)]
    })
    
    # Eigen Contrasts.
    eigens <- shiny::reactive({
      shiny::req(datagroup(), trait_table())
      
      eigen_contrast_dataset(datagroup(), trait_table())
    })
    
    # Compare Selected Group Eigens to Traits in Group
    traits <- shiny::reactive({
      shiny::req(datagroup(), panel_par$sex, panel_par$group, main_par$dataset,
                 group_table(), eigens())
      
      eigen_traits_dataset(datagroup(), panel_par$sex, panel_par$group,
                           group_table(), eigens())
    })
    
    ##############################################################
    contrast_list
  })
}
#' Shiny Module Input for Groups of Contrasts
#' @return nothing returned
#' @rdname contrastGroupServer
#' @export
contrastGroupInput <- function(id) {
  ns <- shiny::NS(id)
  contrastPlotUI(ns("contrast_plot"))
}
#' Shiny Module Output for Groups of Contrasts
#' @return nothing returned
#' @rdname contrastGroupServer
#' @export
contrastGroupOutput <- function(id) {
  ns <- shiny::NS(id)
  contrastPlotOutput(ns("contrast_plot"))
}
#' Shiny Module App for Groups of Contrasts
#' @return nothing returned
#' @rdname contrastGroupServer
#' @export
contrastGroupApp <- function() {
  title <- "Shiny Module Contrast Group"
  
  ui <- function() {
    
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          mainParInput("main_par")
        ),
        shiny::mainPanel(
          mainParOutput("main_par"),
          contrastGroupInput("contrast_group"),
          shiny::fluidRow(
            shiny::column(4, shiny::uiOutput("sex")),
            shiny::column(8, shiny::uiOutput("group"))),
          contrastGroupOutput("contrast_group")
        )
      )
    )
  }
  
  server <- function(input, output, session) {
    main_par <- mainParServer("main_par", traitStats)
    # Contrast Trait Table
    trait_table <- contrastTableServer("contrast_table", main_par,
      traitSignal, traitStats, customSettings)
    # Contrast Traits within Group Table
    group_table <- contrastTableServer("contrast_table", main_par,
      traitSignal, traitStats, customSettings, keepDatatraits)
    # Contrast Groups.
    contrast_list <- contrastGroupServer("contrast_group", input, main_par,
      traitModule, trait_table, group_table)

    # SERVER-SIDE INPUTS
    sexes <- c(B = "Both Sexes", F = "Female", M = "Male", C = "Sex Contrast")
    output$sex <- shiny::renderUI({
      shiny::selectInput("sex", "", as.vector(sexes))
    })
    output$group <- shiny::renderUI({
      shiny::selectizeInput("group", "Group:", NULL)
    })
    shiny::observeEvent(
      shiny::req(datatraits(), main_par$dataset, input$sex),
      {
        # First zero out input$group.
        shiny::updateSelectizeInput(session, "group",
                                    selected = "", server = TRUE)
        # Then set choices.
        shiny::updateSelectizeInput(session, "group", choices = datatraits(),
                                    selected = "", server = TRUE)
      })
    
    datagroup <- shiny::reactive({
      traitModule[shiny::req(main_par$dataset[1])]
    })
    datatraits <- shiny::reactive({
      shiny::req(input$sex, main_par$dataset, datagroup())
      if(is_sex_module(datagroup())) {
        out <- unique(datagroup()[[main_par$dataset[1]]][[input$sex]]$modules$module)
        paste0(main_par$dataset[1], ": ", names(sexes)[match(input$sex, sexes)], "_", out)
      } else {
        paste0(main_par$dataset[1], ": ", unique(datagroup()[[main_par$dataset]]$value$modules$module))
      }
    }, label = "datatraits")
    
    keepDatatraits <- reactive({
      group <- NULL
      if(shiny::isTruthy(input$group))
        group <- input$group
      
      foundr:::keptDatatraits(traitModule, shiny::req(main_par$dataset)[1], group)
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
