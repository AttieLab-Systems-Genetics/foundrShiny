#' Shiny Module Server for Trait Panel
#'
#' @param id identifier for shiny reactive
#' @param traitData,traitSignal,traitStats static data frames
#' @param customSettings list of custom settings
#'
#' @return reactive object 
#' @importFrom shiny column fluidRow h3 moduleServer NS observeEvent reactive
#'             reactiveVal reactiveValues renderUI req selectInput tagList
#'             uiOutput updateSelectInput
#' @importFrom DT renderDataTable
#' @importFrom stringr str_remove str_replace
#' @importFrom foundr is_bestcor summary_bestcor summary_strainstats
#' @export
traitServer <- function(id, main_par,
                            traitData, traitSignal, traitStats,
                            customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # trait inputs
    #   main_par$height: Plot Height
    #   input$mincor: minimum correlation
    #   input$reldataset: relative datasets
    #   input$facet: Facet by strain?
    #   input$strains: Strains to select
    #
    # RETURNS
    #   trait_names()
    
    # MODULES
    # Order Traits by Stats.
    stats_table <- traitOrderServer("stats_table", main_par,
                                    traitStats, customSettings)
    # Key Trait.
    key_trait    <- traitNamesServer("key_trait", main_par, stats_table)
    # Key Trait and Correlation Table.
    cors_table  <- corTableServer("shinyCorTable", input, main_par,
                                  key_trait, traitSignal, customSettings)
    # Related Traits.
    rel_traits   <- traitNamesServer("rel_traits", main_par, cors_table, TRUE)
    # Correlation Plot
    cors_plot   <- corPlotServer("cors_plot", main_par,
                                  cors_table, customSettings)
    # Trait Table.
    trait_table <- traitTableServer("trait_table", input,
      key_trait, rel_traits, traitData, traitSignal, customSettings)
    # Solo and Pairs Plots.
    solos_plot  <- traitSolosServer("shinySolos", input, main_par, trait_table)
    pairs_plot  <- traitPairsServer("shinyPairs", input, main_par, trait_names,
                                   trait_table)
    
    # SERVER-SIDE Inputs
    output$strains <- shiny::renderUI({
      choices <- names(foundr::CCcolors)
      shiny::checkboxGroupInput(ns("strains"), "Strains",
        choices = choices, selected = choices, inline = TRUE)
    })

    # Trait Names.
    trait_names <- shiny::reactive({
      c(shiny::req(key_trait()), rel_traits())
    },
    label = "trait_names")
    
    # Related Datasets.
    output$reldataset <- renderUI({
      datasets <- unique(traitStats$dataset)
      selected <- data_selection()
      shiny::selectInput(ns("reldataset"), "Related Datasets:",
                         datasets, selected, multiple = TRUE)
    })
    data_selection <- shiny::reactiveVal(unique(traitStats$dataset)[1], label = "data_selection")
    shiny::observeEvent(input$reldataset, data_selection(input$reldataset))
    
    
    # Output
    output$text <- shiny::renderUI({
      condition <- customSettings$condition
      if(shiny::isTruthy(condition))
        condition <- tolower(condition)
      else
        condition <- "Condition"
      
      shiny::tagList(
        shiny::h3("Traits"),
        shiny::renderText({
          paste0(
            "This panel examines traits by ",
            condition, ", strain and sex. ",
            "Traits are typically ordered by significance of model terms. ",
            "Response value shows raw data; normed shows values after normal scores preserving mean and SD;",
            "cellmean shows normed values averaged over replicates. ",
            "Selecting Related Traits yields multiple Trait Plots plus Pairs Plots. ",
            "Correlation sorts Related Traits.")
        }))
    })
    output$downtable <- shiny::renderUI({
      if(shiny::req(main_par$plot_table) == "Tables") {
        shiny::radioButtons(ns("buttable"), "Download:",
                            c("Cell Means","Correlations","Stats"), "Cell Means", inline = TRUE)
      }
    })
    output$traitOutput <- shiny::renderUI({
      shiny::tagList(
        switch(shiny::req(main_par$plot_table),
               Plots = {
                 shiny::tagList(
                   traitSolosUI(ns("shinySolos")),
                   # Trait Pairs Plot
                   if(length(shiny::req(trait_names())) > 1)
                     shiny::tagList(
                       shiny::h3("Trait Pairs"),
                       traitPairsUI(ns("shinyPairs"))))
               },
               Tables = {
                 shiny::tagList(
                   traitTableOutput(ns("trait_table")),
                   traitOrderUI(ns("stats_table")))
               }),
        
        # Correlation Plots or Tables
        switch(shiny::req(main_par$plot_table),
               Plots = {
                 if(foundr::is_bestcor(cors_table()))
                   corPlotOutput(ns("cors_plot"))
               },
               Table = corTableOutput(ns("shinyCorTable")))
        )
    })
    ###############################################################
    shiny::reactiveValues(
      postfix = shiny::reactive({
        filename <- stringr::str_replace(trait_names()[1], ": ", "_")
        if(shiny::req(main_par$plot_table) == "Tables")
          filename <- paste0(stringr::str_remove(input$buttable, " "), "_",
                             filename)
        filename
      }),
      plotObject = shiny::reactive({
        shiny::req(solos_plot(), main_par$height)
        
        print(solos_plot())
        if(length(shiny::req(trait_names())) > 1)
          print(pairs_plot())
        if(foundr::is_bestcor(cors_table()) & shiny::isTruthy(cors_table()))
          print(cors_table())
      }),
      tableObject = shiny::reactive({
        shiny::req(trait_table())
        switch(shiny::req(input$buttable),
               "Cell Means" = summary(trait_table()),
               Correlations = foundr::summary_bestcor(
                 mutate_datasets(cors_table(), customSettings$dataset), 0.0),
               Stats = foundr::summary_strainstats(stats_table(),
                                                   threshold = c(deviance = 0, p = 1)))
      })
    )
  })
}
#' Shiny Module Input for Trait Panel
#' @return nothing returned
#' @rdname traitServer
#' @export
traitInput <- function(id) { # 4:Order, 8:Traits
  ns <- shiny::NS(id)
  traitNamesUI(ns("key_trait"))
}
#' Shiny Module UI for Trait Panel
#' @return nothing returned
#' @rdname traitServer
#' @export
traitUI <- function(id) { # Related Datasets and Traits
  ns <- shiny::NS(id)
  shiny::tagList(
    # Related Datasets and Traits.
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("reldataset"))),
      shiny::column(6, traitNamesUI(ns("rel_traits"))))
  )
}
#' Shiny Module Output for Trait Panel
#' @return nothing returned
#' @rdname traitServer
#' @export
traitOutput <- function(id) { # Plots or Tables
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("text")),
    shiny::fluidRow(
      shiny::column(6, traitTableUI(ns("trait_table"))), # Response
      shiny::column(6, shiny::uiOutput(ns("downtable")))),
    shiny::fluidRow(
      shiny::column(9, shiny::uiOutput(ns("strains"))),
      shiny::column(3, shiny::checkboxInput(ns("facet"), "Facet by strain?", TRUE))),
    shiny::uiOutput(ns("traitOutput")))
}
#' Shiny Module App for Trait Panel
#' @return nothing returned
#' @rdname traitServer
#' @export
traitApp <- function() {
  title <- "Test Shiny Trait Panel"
  
  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fluidRow(
          shiny::column(3, mainParInput("main_par")), # dataset
          shiny::column(3, mainParUI("main_par")), # order
          shiny::column(6, traitInput("trait_list"))), # key_trait
        traitUI("trait_list"),
        border_line(),
        mainParOutput("main_par"), # plot_table, height
        downloadOutput("download")
      ),
      shiny::mainPanel(
        traitOutput("trait_list")
      )
    )
  )
  server <- function(input, output, session) {
    # CALL MODULES
    main_par <- mainParServer("main_par", traitStats)
    trait_list <- traitServer("trait_list", main_par,
      traitData, traitSignal, traitStats, customSettings)
    downloadServer("download", "Trait", main_par, trait_list)
  }
  
  shiny::shinyApp(ui = ui, server = server)  
}
