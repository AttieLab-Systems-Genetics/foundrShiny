#' panel App for foundr package
#'
#' @param id identifier for shiny reactive
#' @param traitData,traitSignal,traitStats,traitModule static objects
#' @param customSettings list of custom settings
#' @return reactive server
#' 
#' @importFrom shiny checkboxGroupInput hideTab observeEvent reactive
#'             reactiveVal reactiveValues renderUI req showTab updateTabsetPanel
#' @importFrom grDevices dev.off pdf
#' @importFrom utils write.csv
#' @export
panelApp <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Panel App"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        panelInput("panel")
      ),
      shiny::mainPanel(
        panelOutput("panel")
      )
    )
  )
  server <- function(input, output, session) {
    panel_list <- panelServer("panel",
                 traitData, traitSignal, traitStats,
                 customSettings, traitModule)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname panelApp
panelServer <- function(id,
                   traitData = NULL, traitSignal = NULL, traitStats = NULL,
                   customSettings = NULL, traitModule = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # CALL MODULES
    main_par <- mainParServer("main_par", traitStats)
    trait_list <- traitServer("tabTraits", main_par,
      traitData, traitSignal, traitStats, customSettings)
    contrast_list <- contrastServer("tabContrasts", main_par,
      traitSignal, traitStats, traitModule, customSettings)
    stats_list <- statsServer("tabStats", main_par,
      traitStats, customSettings)
    time_list <- timeServer("tabTimes", main_par,
      traitData, traitSignal, traitStats)
    aboutServer("about", customSettings$help)
    
    downloadServer("download", "Foundr", main_par, panel_list)
    panel_list <- shiny::reactiveValues(
      panel       = shiny::reactive(shiny::req(input$tabpanel)),
      height      = shiny::reactive({
        switch(shiny::req(input$tabpanel),
               Traits    = shiny::req(trait_list$height()),
               Contrasts = shiny::req(contrast_list$height()),
               Stats     = shiny::req(stats_list$height()),
               Times     = shiny::req(time_list$height()))
      }),
      postfix     = shiny::reactive({
        switch(shiny::req(input$tabpanel),
               Traits    = shiny::req(trait_list$postfix()),
               Contrasts = shiny::req(contrast_list$postfix()),
               Stats     = shiny::req(stats_list$postfix()),
               Times     = shiny::req(time_list$postfix()))
      }),
      plotObject  = shiny::reactive({
        switch(shiny::req(input$tabpanel),
               Traits    = shiny::req(trait_list$plotObject()),
               Contrasts = shiny::req(contrast_list$plotObject()),
               Stats     = shiny::req(stats_list$plotObject()),
               Times     = shiny::req(time_list$plotObject()))
      }),
      tableObject = shiny::reactive({
        switch(shiny::req(input$tabpanel),
               Traits    = shiny::req(trait_list$tableObject()),
               Contrasts = shiny::req(contrast_list$tableObject()),
               Stats     = shiny::req(stats_list$tableObject()),
               Times     = shiny::req(time_list$tableObject()))
      })
    )
    
    # Does project have time data? If not, hide those tabs.
    has_time_data <- length(timetraitsall(traitSignal) > 0)
    
    # Side Input
    output$sideInput <- shiny::renderUI({
      shiny::req(input$tabpanel)
      
      # Tab-specific side panel.
      if(input$tabpanel != "About") {
        shiny::tagList(
          shiny::fluidRow(
            shiny::column(6, mainParInput(ns("main_par"))), # dataset
            if(input$tabpanel %in% c("Traits", "Times"))
              shiny::column(6, mainParUI(ns("main_par"))), # order
          ),
          if(input$tabpanel %in% c("Traits","Times","Contrasts")) {
            switch(input$tabpanel, # key_trait and 
                   Traits    = traitInput(ns("tabTraits")), # rel_dataset, rel_traits
                   Contrasts = contrastInput(ns("tabContrasts")), # time_unit
                   Times     = if(has_time_data)
                     timeInput(ns("tabTimes"))) # time_unit, response
          },
          border_line(),
          shiny::fluidRow(
            shiny::column(6, mainParOutput1(ns("main_par"))), # plot_table
            # Within-panel call of panelPar.
            # panelParOutput(ns("panel_par")) # height or table
            shiny::column(6, switch(input$tabpanel,
                                    Traits    = traitUI(ns("tabTraits")),
                                    Contrasts = contrastUI(ns("tabContrasts")),
                                    Stats     = statsUI(ns("tabStats")),
                                    Times     = if(has_time_data)
                                      timeUI(ns("tabTimes")))
            ),
          ),
          downloadOutput(ns("download"))
        )
      }
    })
    # Main Output
    output$mainOutput <- shiny::renderUI({
      shiny::tabsetPanel(
        type = "tabs", header = "", selected = "Traits", id = ns("tabpanel"),
        shiny::tabPanel("Traits",    traitOutput(ns("tabTraits"))),
        if(has_time_data)
          shiny::tabPanel("Contrasts", contrastOutput(ns("tabContrasts"))),
        shiny::tabPanel("Stats",     statsOutput(ns("tabStats"))),
        if(has_time_data)
          shiny::tabPanel("Times",     timeOutput(ns("tabTimes"))),
        shiny::tabPanel("About",     aboutOutput(ns("about")))
      )
    })
    # Return
    panel_list
  })
}
#' @export
#' @rdname panelApp
panelInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("sideInput"))
}
#' @export
#' @rdname panelApp
panelOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("mainOutput"))
}
