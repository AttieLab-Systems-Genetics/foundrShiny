#' foundr App for foundr package
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
#' @importFrom foundr timetraitsall
#' @export
foundrApp <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Foundr App"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        foundrInput("foundr")
      ),
      shiny::mainPanel(
        foundrOutput("foundr")
      )
    )
  )
  server <- function(input, output, session) {
    foundrServer("foundr",
                 traitData, traitSignal, traitStats,
                 customSettings, traitModule)
  }
  shiny::shinyApp(ui, server)
}
#' @export
#' @rdname foundrApp
foundrServer <- function(id,
                   traitData = NULL, traitSignal = NULL, traitStats = NULL,
                   customSettings = NULL, traitModule = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # CALL MODULES
    entry <- entryServer("entry", customSettings)
    main_par <- mainParServer("main_par", traitStats)
    trait_list <- traitServer("tabTraits", main_par,
      traitData, traitSignal, traitStats, customSettings)
    contrast_list <- contrastServer("tabContrasts", main_par,
      traitSignal, traitStats, traitModule, customSettings)
    stats_list <- statsServer("tabStats", main_par,
      traitStats, customSettings)
    time_list <- timeServer("tabTimes", main_par,
      traitData, traitSignal, traitStats)
    aboutServer("about", customSettings$help, entry)
    downloadServer("download", "Foundr", main_par, download_list)
    download_list <- shiny::reactiveValues(
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
    has_time_data <- length(foundr::timetraitsall(traitSignal) > 0)
    
    # Hide all tabs unless Entry word provided.
    shiny::observeEvent(
      shiny::tagList(input$height, entry(), input$tabpanel), {
        shiny::showTab("tabpanel", target = "About")
        if(entry()) {
          shiny::showTab("tabpanel", target = "Traits")
          shiny::showTab("tabpanel", target = "Stats")
          # Times and Contrasts tabs hidden for calcium study for now.
          if(has_time_data) {
            shiny::showTab("tabpanel", target = "Times")
            shiny::showTab("tabpanel", target = "Contrasts")
          } else {
            shiny::hideTab("tabpanel", target = "Times")
            shiny::hideTab("tabpanel", target = "Contrasts")
          }
        } else {
          shiny::hideTab("tabpanel", target = "Traits")
          shiny::hideTab("tabpanel", target = "Times")
          shiny::hideTab("tabpanel", target = "Contrasts")
          shiny::hideTab("tabpanel", target = "Stats")
          #        shiny::hideTab("tabpanel", target = "About")
        }
      })
    
    # Side Input
    output$sideInput <- shiny::renderUI({
      shiny::req(input$tabpanel)
      
      # Tab-specific side panel.
      if(input$tabpanel == "About") {
        if(!shiny::isTruthy(entry())) {
          shiny::tagList(
            entryInput(ns("entry")),
            entryOutput(ns("entry"))
          )
        }
      } else {
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
                                    #Contrasts = contrastUI(ns("tabContrasts")), # not working
                                    #Stats     = statsUI(ns("tabStats")), # not working
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
        type = "tabs", header = "", selected = "About", id = ns("tabpanel"),
        shiny::tabPanel("Traits",    traitOutput(ns("tabTraits"))),
        shiny::tabPanel("Contrasts", contrastOutput(ns("tabContrasts"))),
        shiny::tabPanel("Stats",     statsOutput(ns("tabStats"))),
        shiny::tabPanel("Times",     timeOutput(ns("tabTimes"))),
        shiny::tabPanel("About",     aboutOutput(ns("about")))
      )
    })
    shiny::observeEvent(shiny::isTruthy(entry()), {
      shiny::updateTabsetPanel(session, "tabpanel", selected = "Traits")
    })
  })
}
#' @export
#' @rdname foundrApp
foundrInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("sideInput"))
}
#' @export
#' @rdname foundrApp
foundrOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("mainOutput"))
}
