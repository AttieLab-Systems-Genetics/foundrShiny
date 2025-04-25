#' Trait Panel App
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
traitApp <- function() {
  title <- "Test Shiny Trait Panel"
  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fluidRow(
          shiny::column(6, mainParInput("main_par")), # dataset
          shiny::column(6, mainParUI("main_par"))), # order
        traitInput("trait_list"), # key_trait, rel_dataset, rel_traits
        border_line(),
        shiny::fluidRow(
          shiny::column(6, mainParOutput1("main_par")), # plot_table
          shiny::column(6, traitUI("trait_list"))), # height or table
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
#' @rdname traitApp
#' @export
traitServer <- function(id, main_par,
                            traitData, traitSignal, traitStats,
                            customSettings = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    panel_par <- panelParServer("panel_par", main_par, traitStats, "trait")
    # Order Traits by Stats.
    stats_table <- traitOrderServer("stats_table", main_par,
                                    traitStats, customSettings)
    # Key Trait and Correlation Table.
    key_trait   <- traitNamesServer("key_trait", main_par, stats_table)
    cors_table  <- corTableServer("cors_table", main_par,
                                  key_trait, traitSignal, customSettings)
    # Related Traits.
    rel_traits  <- traitNamesServer("rel_traits", main_par, cors_table, TRUE)
    cors_plot   <- corPlotServer("cors_plot", main_par,
                                  cors_table, customSettings)
    # Trait Table.
    trait_table <- traitTableServer("trait_table", panel_par,
      key_trait, rel_traits, traitData, traitSignal, customSettings)
    # Solo and Pairs Plots.
    trait_plot  <- traitSolosServer("trait_plot", panel_par, main_par,
      trait_table)
    pairs_plot  <- traitPairsServer("pairs_plot", panel_par, main_par,
      trait_names, trait_table)
    
    # Trait Names.
    trait_names <- shiny::reactive(c(shiny::req(key_trait()), rel_traits()),
      label = "trait_names")
    
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
    output$plot_table <- shiny::renderUI({
      shiny::req(main_par$plot_table)
      shiny::tagList(
        switch(main_par$plot_table,
               Plots = {
                 shiny::tagList(
                   traitSolosOutput(ns("trait_plot")),
                   # Trait Pairs Plot
                   if(length(shiny::req(trait_names())) > 1)
                     shiny::tagList(
                       shiny::h3("Trait Pairs"),
                       traitPairsOutput(ns("pairs_plot"))))
               },
               Tables = {
                 shiny::tagList(
                   traitTableOutput(ns("trait_table")),
                   traitOrderUI(ns("stats_table")))
               }),
        
        # Correlation Plots or Tables
        switch(main_par$plot_table,
               Plots = {
                 if(foundr::is_bestcor(cors_table()))
                   corPlotOutput(ns("cors_plot"))
               },
               Tables = corTableOutput(ns("cors_table")))
      )
    })
    ###############################################################
    shiny::reactiveValues(
      panel       = shiny::reactive("Traits"),
      height      = shiny::reactive(panel_par$height),
      postfix     = shiny::reactive({
        filename <- stringr::str_replace(trait_names()[1], ": ", "_")
        if(shiny::req(main_par$plot_table) == "Tables")
          filename <- paste0(stringr::str_remove(panel_par$table, " "), "_",
                             filename)
        filename
      }),
      plotObject  = shiny::reactive({
        shiny::req(trait_plot())
        
        print(trait_plot())
        if(length(shiny::req(trait_names())) > 1)
          print(pairs_plot())
        if(foundr::is_bestcor(cors_table()) & shiny::isTruthy(cors_table()))
          print(shiny::req(cors_plot()))
      }),
      tableObject = shiny::reactive({
        shiny::req(trait_table())
        switch(shiny::req(panel_par$table),
               "Cell Means" = summary(trait_table()),
               Correlations = foundr::summary_bestcor(
                 mutate_datasets(cors_table(), customSettings$dataset), 0.0),
               Stats = foundr::summary_strainstats(stats_table(),
                 threshold = c(deviance = 0, p = 1)))
      })
    )
  })
}
#' @rdname traitApp
#' @export
traitInput <- function(id) { # 4:Order, 8:Traits
  ns <- shiny::NS(id)
  shiny::tagList(
    # Key Dataset and Trait.
    traitNamesUI(ns("key_trait")), # key_trait
    # Related Datasets and Traits.
    shiny::fluidRow(
      shiny::column(6, corTableInput(ns("cors_table"))), # rel_dataset
      shiny::column(6, traitNamesUI(ns("rel_traits")))), # rel_traits
    traitTableUI(ns("trait_table"))) # response
}
#' @rdname traitApp
#' @export
traitUI <- function(id) { # height or table
  ns <- shiny::NS(id)
  panelParOutput(ns("panel_par")) # height or table
}
#' @rdname traitApp
#' @export
traitOutput <- function(id) { # Plots or Tables
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("text")),
    panelParInput(ns("panel_par")), # strains, facet,
    shiny::uiOutput(ns("plot_table")))
}
