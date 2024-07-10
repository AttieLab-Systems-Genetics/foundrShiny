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
    
    contrast_list <- contrastPlotServer("contrast_plot", panel_par, main_par,
      contrastTable, customSettings, modTitle)
    
    groupname <- stringr::str_to_title(customSettings$group)
    if(!length(groupname)) groupname <- "Group"
    
    modTitle <- shiny::reactive({
      if(shiny::isTruthy(input$group)) 
        paste("Eigentrait Contrasts for", groupname, input$group)
      else
        paste0("Eigentrait Contrasts across ", groupname, "s")
    })
    
    contrastTable <- shiny::reactive({
      if(shiny::isTruthy(input$group)) traits() else eigens()      
    })
    # Contrasts among Eigentraits.
    eigens <- shiny::reactive({
      shiny::req(datagroup(), trait_table())
      eigen_contrast_dataset(datagroup(), trait_table())
    })
    # Contrasts among Traits in Group.
    traits <- shiny::reactive({
      shiny::req(datagroup(), panel_par$sex, input$group, main_par$dataset,
                 group_table(), eigens())
      eigen_traits_dataset(datagroup(), panel_par$sex, input$group,
                           group_table(), eigens())
    })
    
    # Restrict `traitModule` to datasets in `trait_table()`
    datagroup <- shiny::reactive({
      traitModule[shiny::req(main_par$dataset[1])]
    })
    sexes <- c(B = "Both Sexes", F = "Female", M = "Male", C = "Sex Contrast")
    datatraits <- shiny::reactive({
      shiny::req(panel_par$sex, main_par$dataset, datagroup())
      dataset <- main_par$dataset[1]
      if(is_sex_module(datagroup())) {
        sex_name <- names(sexes)[match(panel_par$sex, sexes)]
        out <- unique(datagroup()[[dataset]][[panel_par$sex]]$modules$module)
        paste0(dataset, ": ", sex_name, "_", out)
      } else {
        paste0(dataset, ": ", unique(datagroup()[[dataset]]$value$modules$module))
      }
    }, label = "datatraits")
    
    # Server-side INPUTS
    output$group <- shiny::renderUI({
      shiny::selectizeInput(ns("group"), "Group:", NULL)
    })
    shiny::observeEvent(
      shiny::req(datatraits(), main_par$dataset, panel_par$sex), {
      # First zero out input$group.
      shiny::updateSelectizeInput(session, "group",
                                  selected = "", server = TRUE)
      # Then set choices.
      shiny::updateSelectizeInput(session, "group", choices = datatraits(),
                                  selected = "", server = TRUE)
    })
    shiny::observeEvent(input$group, {
      contrast_list$group <- input$group
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
  contrastPlotUI(ns("contrast_plot")) # ordername, interact
}
#' Shiny Module UI for Groups of Contrasts
#' @return nothing returned
#' @rdname contrastGroupServer
#' @export
contrastGroupUI <- function(id) { # group
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("group"))
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
          mainParInput("main_par") # dataset
        ),
        shiny::mainPanel(
          mainParOutput("main_par"), # plot_table, height
          contrastGroupInput("contrast_group"), # ordername, interact
          shiny::fluidRow(
            shiny::column(4, panelParUI("panel_par")), # sex
            shiny::column(8, contrastGroupUI("contrast_group"))), # group
          contrastGroupOutput("contrast_group") # volsd, volvert, rownames
        )
      )
    )
  }
  server <- function(input, output, session) {
    main_par <- mainParServer("main_par", traitStats)
    panel_par <- panelParServer("panel_par", main_par, traitStats)
    # Contrast Trait Table
    trait_table <- contrastTableServer("contrast_table", main_par,
      traitSignal, traitStats, customSettings)
    # Contrast Traits within Group Table
    group_table <- contrastTableServer("contrast_table", main_par,
      traitSignal, traitStats, customSettings, keepDatatraits)
    # Contrast Groups.
    contrast_list <- contrastGroupServer("contrast_group", panel_par, main_par,
      traitModule, trait_table, group_table)

    keepDatatraits <- reactive({
      group <- NULL
      if(shiny::isTruthy(input$group))
        group <- input$group
      foundr:::keptDatatraits(traitModule, shiny::req(main_par$dataset)[1], group)
    })
  }
  shiny::shinyApp(ui = ui, server = server)
}
