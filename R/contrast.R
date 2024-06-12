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
    
    # Identify all Time Traits.
    timetrait_all <- timetraitsall(traitSignal)
    # Subset Stats to time traits.
    traitStatsTime <- time_trait_subset(traitStats, timetrait_all)
    
    # MODULES
    # Contrast Group Table. Note reuse of `id` for `contrastTableServer`.
    mods_table <- contrastTableServer("contrast_table", input, main_par,
      traitSignal, traitStats, customSettings)
    # Contrast Trait Table
    trait_table <- contrastTableServer("contrast_table", input, main_par,
      traitSignal, traitStats, customSettings, keepDatatraits)
    # Contrast Trait Plots by Sex
    sex_list <- contrastSexServer("contrast_sex", input, main_par,
      mods_table, customSettings)
    # Contrast Time Trait Table
    times_table <- contrastTableServer("times_table", input, main_par,
      traitSignal, traitStatsTime, customSettings)
    # Contrast Time Traits
    contrast_time <- contrastTimeServer("contrast_time", input, main_par,
      traitSignal, traitStatsTime, times_table, customSettings)
    # Contrast Time Plots and Tables
    time_list <- timePlotServer("shinyTimePlot", input, main_par,
      traitSignal, contrast_time)
    # Contrast Groups.
    group_list <- contrastGroupServer("contrast_group", input, main_par,
      traitModule, mods_table, trait_table)
    
    # SERVER-SIDE Inputs
    output$strains <- shiny::renderUI({
      choices <- names(foundr::CCcolors)
      shiny::checkboxGroupInput(ns("strains"), "Strains",
                                choices = choices, selected = choices, inline = TRUE)
    })
    output$butby <- shiny::renderUI({
      if(length(timetraits_dataset())) {
        buttons <- c("Sex", "Group", "Time")
      } else {
        buttons <- c("Sex", "Group")
      }
      shiny::radioButtons(ns("contrast"), "Contrast by ...",
                          buttons, inline = TRUE)
    })
    contr_selection <- shiny::reactiveVal(NULL, label = "contr_selection")
    shiny::observeEvent(input$contrast, contr_selection(input$contrast))
    
    timetraits_dataset <- shiny::reactive({
      shiny::req(main_par$dataset)
      
      foundr::timetraitsall(dplyr::filter(traitSignal, dataset %in% main_par$dataset))
    })
    sexes <- c(B = "Both Sexes", F = "Female", M = "Male", C = "Sex Contrast")
    output$sex <- shiny::renderUI({
      shiny::selectInput(ns("sex"), "", as.vector(sexes))
    })
    output$group <- shiny::renderUI({
      shiny::selectizeInput(ns("group"), "Group:", NULL)
    })
    shiny::observeEvent(
      shiny::req(datatraits(), main_par$dataset, input$sex, contr_selection()),
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
      
      if(foundr:::is_sex_module(datagroup())) {
        out <- unique(
          datagroup()[[main_par$dataset[1]]][[input$sex]]$modules$module)
        paste0(main_par$dataset[1], ": ",
          names(sexes)[match(input$sex, sexes)], "_", out)
      } else {
        paste0(main_par$dataset[1], ": ",
          unique(datagroup()[[main_par$dataset]]$value$modules$module))
      }
    }, label = "datatraits")
    
    keepDatatraits <- reactive({
      group <- NULL
      if(shiny::isTruthy(input$group))
        group <- input$group
      
      foundr:::keptDatatraits(traitModule, shiny::req(main_par$dataset)[1],
                              group)
    })
    
    # Input
    output$shinyInput <- shiny::renderUI({
      shiny::req(contr_selection())
      if(contr_selection() == "Time")
        contrastTimeInput(ns("contrast_time")) # Traits
    })
    output$shinyUI <- shiny::renderUI({
      shiny::req(contr_selection())
      if(contr_selection() == "Time") {
        contrastTimeUI(ns("contrast_time")) # Time Unit
      }
    })
    
    # Output
    output$shinyOutput <- shiny::renderUI({
      shiny::req(contr_selection())
      shiny::tagList(
        shiny::uiOutput(ns("text")),
        
        if(contr_selection() == "Group") {
          contrastGroupInput(ns("contrast_group"))
        },
        if(contr_selection() == "Time") {
          shiny::fluidRow(
            shiny::column(9, shiny::uiOutput(ns("strains"))),
            shiny::column(3, shiny::checkboxInput(ns("facet"),
                                                  "Facet by strain?", TRUE)))
        } else { # Sex, Group
          shiny::fluidRow(
            shiny::column(4, shiny::uiOutput(ns("sex"))),
            shiny::column(8, 
                          switch(contr_selection(),
                                 Sex    = contrastSexUI(ns("contrast_sex")),
                                 Group = shiny::uiOutput(ns("group")))))
        },
        
        switch(contr_selection(),
               Time = {
                 shiny::tagList(
                   timePlotUI(ns("shinyTimePlot")),
                   timePlotOutput(ns("shinyTimePlot")))
               },
               Sex = contrastSexOutput(ns("contrast_sex")),
               Group = contrastGroupOutput(ns("contrast_group"))))
    })
    
    output$text <- shiny::renderUI({
      condition <- customSettings$condition
      if(shiny::isTruthy(condition))
        condition <- stringr::str_to_title(condition)
      else
        condition <- "Condition"
      
      shiny::tagList(
        shiny::h3(paste(condition, "Contrasts")),
        shiny::renderText({
          out <- paste0(
            "This panel examines contrasts (differences or ratios) of ",
            condition, " means by strain and sex.",
            "These may be viewed by sex or averaged over sex",
            " (Both Sexes) or by contrast of Female - Male",
            " (Sex Contrast).")
          if(shiny::req(contr_selection()) == "Time")
            out <- paste(out, "Contrasts over time are by trait.")
          if(shiny::req(contr_selection()) == "Group")
            out <- paste(out, "WGCNA modules by dataset and sex have",
                         "power=6, minSize=4.",
                         "Select a Group to see module members.")
          out
        }))
    })
    ###############################################################
    shiny::reactiveValues(
      postfix     = shiny::reactive({
        switch(shiny::req(contr_selection()),
               Sex   = sex_list$postfix(),
               Group = group_list$postfix(),
               Time  = time_list$postfix())
      }),
      plotObject  = shiny::reactive({
        switch(shiny::req(contr_selection()),
               Sex   = sex_list$plotObject(),
               Group = group_list$plotObject(),
               Time  = time_list$plotObject())
      }),
      tableObject = shiny::reactive({
        switch(shiny::req(contr_selection()),
               Sex   = sex_list$tableObject(),
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
contrastInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("shinyInput")) # Order, Traits (if butby == "Time")
}
#' Shiny Module Input for Contrast Panel
#' @return nothing returned
#' @rdname contrastServer
#' @export
contrastUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("shinyUI")), # Time Unit (if butby == "Time")
    shiny::uiOutput(ns("butby")))
}
#' Shiny Module Output for Contrast Panel
#' @return nothing returned
#' @rdname contrastServer
#' @export
contrastOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("shinyOutput"))
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
          shiny::fluidRow(
            shiny::column(6, mainParInput("main_par")),
            shiny::column(6, contrastInput("shinyPanel"))),
          contrastUI("shinyPanel"),
          shiny::hr(style="border-width:5px;color:black;background-color:black"),
          mainParUI("main_par"),
          downloadOutput("download")
        ),
        shiny::mainPanel(
          contrastOutput("shinyPanel")
        )
      )
    )
  }
  server <- function(input, output, session) {
    
    #  shiny::onStop(function() {RSQLite::dbDisconnect(db)})
    
    # CALL MODULES
    main_par <- mainParServer("main_par", traitStats)
    contrast_list <- contrastServer("shinyPanel", main_par,
      traitSignal, traitStats, traitModule, customSettings)
    downloadServer("download", "Contrast", main_par, contrast_list)
  }
  
  shiny::shinyApp(ui = ui, server = server)  
}
