#' Shiny Module Server for Contrast Panel
#'
#' @param id identifier for shiny reactive
#' @param main_par reactive arguments 
#' @param traitSignal,traitStats,traitModule static objects
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny column fluidRow h3 isTruthy moduleServer NS radioButtons
#'             reactive reactiveVal renderText renderUI tagList uiOutput
#' @importFrom stringr str_to_title
#' @export
#'
contrastServer <- function(id, main_par,
                               traitSignal, traitStats, traitModule,
                               customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    groupname <- stringr::str_to_title(customSettings$group)
    if(!length(groupname)) groupname <- "Group"
    
    panel_par <- panelParServer("panel_par", main_par, traitStats, "contrast")
    # Contrast Tables.
    trait_table <- contrastTableServer("trait_table", main_par,
      traitSignal, traitStats, customSettings)
    group_table <- contrastTableServer("trait_table", main_par,
      traitSignal, traitStats, customSettings, keepDatatraits)
    stats_time_table <- time_trait_subset(traitStats,
      timetraitsall(traitSignal))
    time_table <- contrastTableServer("time_table", main_par,
      traitSignal, stats_time_table, customSettings)
    # Contrast over Sex by Trait.
    trait_list <- contrastTraitServer("trait_list", panel_par, main_par,
      trait_table, customSettings)
    # Contrast over Sex by Group.
    group_list <- contrastGroupServer("group_list", panel_par, main_par,
      traitModule, trait_table, group_table, customSettings)
    # Contrast over Sex by Trait over Time.
    contrast_time <- contrastTimeServer("contrast_time", panel_par, main_par,
      traitSignal, stats_time_table, time_table, customSettings)
    time_list <- timePlotServer("time_list", panel_par, main_par,
      traitSignal, contrast_time)
    
    # SERVER-SIDE Inputs
    output$contrast_type <- shiny::renderUI({
      if(length(timetraits_dataset())) {
        buttons <- c(groupname, "Trait", "Time")
      } else {
        buttons <- c(groupname, "Trait")
      }
      shiny::radioButtons(ns("contrast_type"), "Contrast by ...",
                          buttons, inline = TRUE)
    })
    contrast_type <- shiny::reactiveVal(NULL, label = "contrast_type")
    shiny::observeEvent(input$contrast_type, contrast_type(
      # Change back to Group from groupname for internal code.
      ifelse(input$contrast_type == groupname, "Group", input$contrast_type)))
    
    timetraits_dataset <- shiny::reactive({
      datasets <- shiny::req(main_par$dataset)
      foundr::timetraitsall(dplyr::filter(traitSignal, dataset %in% datasets))
    })

    keepDatatraits <- reactive({
      group <- NULL
      if(shiny::isTruthy(group_list$group)) group <- group_list$group
      dataset <- shiny::req(main_par$dataset)[1]
      foundr:::keptDatatraits(traitModule, dataset, group)
    })
    
    # UI Components
    output$contrast_input <- shiny::renderUI({
      shiny::req(contrast_type())
      if(contrast_type() == "Time") {
        shiny::tagList(
          contrastTimeInput(ns("contrast_time")), # traits
          contrastTimeUI(ns("contrast_time")) # time_unit
        )
      }
    })
    output$contrast_output <- shiny::renderUI({
      shiny::req(contrast_type())
      shiny::tagList(
        shiny::uiOutput(ns("text")),
        if(contrast_type() == "Time") {
          panelParInput(ns("panel_par")) # strains, facet
        } else { # Trait, Group
          shiny::fluidRow(
            shiny::column(4, panelParUI(ns("panel_par"))), # sex
            shiny::column(8, switch(contrast_type(), # ordername, interact
              Trait = contrastTraitInput(ns("trait_list")),
              Group = contrastGroupInput(ns("group_list")))))
        },
        if(contrast_type() == "Group") {
          contrastGroupUI(ns("group_list"))
        },
        switch(contrast_type(),
               Time  = timePlotOutput(ns("time_list")),
               Trait = contrastTraitOutput(ns("trait_list")),
               Group = contrastGroupOutput(ns("group_list"))))
    })
    
    output$text <- shiny::renderUI({
      condition <- customSettings$condition
      condition <- ifelse(shiny::isTruthy(condition),
        stringr::str_to_title(condition), "Condition")
      
      shiny::tagList(
        shiny::h3(paste(condition, "Contrasts")),
        shiny::renderText({
          out <- paste0(
            "This panel examines contrasts (differences or ratios) of ",
            condition, " means by strain and sex. ",
            "These may be viewed by sex or averaged over sex",
            " (Both Sexes) or by contrast of Female - Male",
            " (Sex Contrast).")
          if(shiny::req(contrast_type()) == "Time")
            out <- paste(out, "Contrasts over time are by trait.")
          if(shiny::req(contrast_type()) == "Group")
            out <- paste(out, "WGCNA modules by dataset and sex have",
                         "power=6, minSize=4.",
                         "Select a", groupname, "to see module members.")
          out
        }))
    })
    ###############################################################
    shiny::reactiveValues(
      panel       = shiny::reactive("Contrasts"),
      postfix     = shiny::reactive({
        switch(shiny::req(contrast_type()),
               Trait = trait_list$postfix(),
               Group = group_list$postfix(),
               Time  = time_list$postfix())
      }),
      plotObject  = shiny::reactive({
        switch(shiny::req(contrast_type()),
               Trait = trait_list$plotObject(),
               Group = group_list$plotObject(),
               Time  = time_list$plotObject())
      }),
      tableObject = shiny::reactive({
        switch(shiny::req(contrast_type()),
               Trait = trait_list$tableObject(),
               Group = group_list$tableObject(),
               Time  = time_list$tableObject())
      })
    )
  })
}
#' Shiny Module Input for Contrast Panel
#' @return nothing returned
#' @rdname contrastServer
#' @export
contrastInput <- function(id) { # key_trait, time_unit, contrast_type
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("contrast_input")), # key_trait, time_unit
    shiny::uiOutput(ns("contrast_type")) # contrast_type
  )
}
#' Shiny Module Input for Contrast Panel
#' @return nothing returned
#' @rdname contrastServer
#' @export
contrastUI <- function(id) { # height or table
  ns <- shiny::NS(id)
  panelParOutput(ns("panel_par")) # height or table
}
#' Shiny Module Output for Contrast Panel
#' @return nothing returned
#' @rdname contrastServer
#' @export
contrastOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("contrast_output"))
}
#' Shiny Module App for Contrast Panel
#' @return nothing returned
#' @rdname contrastServer
#' @export
contrastApp <- function() {
  title <- "Test Shiny Contrast Trait Panel"
  ui <- function() {
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          mainParInput("main_par"), # dataset
          contrastInput("contrast_list"),
          border_line(),
          shiny::fluidRow(
            shiny::column(6, mainParOutput1("main_par")), # plot_table
            shiny::column(6, contrastUI("contrast_list"))), # height or table
          downloadOutput("download")
        ),
        shiny::mainPanel(
          contrastOutput("contrast_list")
        )
      )
    )
  }
  server <- function(input, output, session) {
    main_par <- mainParServer("main_par", traitStats)
    contrast_list <- contrastServer("contrast_list", main_par,
      traitSignal, traitStats, traitModule, customSettings)
    downloadServer("download", "Contrast", main_par, contrast_list)
  }
  shiny::shinyApp(ui = ui, server = server)  
}
