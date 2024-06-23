#' Shiny Server for foundr Package
#'
#' @param id identifier for shiny reactive
#' @param traitData,traitSignal,traitStats,traitModule static objects
#' @param customSettings list of custom settings
#'
#' @return reactive server
#' 
#' @export
#' 
#' @importFrom shiny checkboxGroupInput hideTab observeEvent reactive
#'             reactiveVal renderUI req showTab
#' @importFrom grDevices dev.off pdf
#' @importFrom utils write.csv
#' @importFrom foundr timetraitsall
#'
foundrServer <- function(id,
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
    downloadServer("download", "Contrast", main_par, download_list)

    download_list <- shiny::reactiveValues(
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

    # Entry key
    entrykey <- shiny::reactive({
      out <- !shiny::isTruthy(customSettings$entrykey)
      if(!out & shiny::isTruthy(input$appEntry)) {
        out <- (input$appEntry == customSettings$entrykey)
      }
      out
    })
    
    # Hide all tabs unless Entry word provided.
    shiny::observeEvent(
      shiny::tagList(input$height, entrykey(), input$tabpanel),
      {
        if(shiny::isTruthy(entrykey())) {
          shiny::showTab("tabpanel", target = "Traits")
          if(length(timetraits_all())) {
            shiny::showTab("tabpanel", target = "Times")
            # Hidden for calcium study for now.
            shiny::showTab("tabpanel", target = "Contrasts")
          } else {
            shiny::hideTab("tabpanel", target = "Times")
            # Hidden for calcium study for now.
            shiny::hideTab("tabpanel", target = "Contrasts")
          }
          shiny::showTab("tabpanel", target = "Stats")
          shiny::showTab("tabpanel", target = "About")
        } else {
          shiny::hideTab("tabpanel", target = "Traits")
          shiny::hideTab("tabpanel", target = "Times")
          shiny::hideTab("tabpanel", target = "Contrasts")
          shiny::hideTab("tabpanel", target = "Stats")
          shiny::hideTab("tabpanel", target = "About")
        }
      })
    timetraits_all <- shiny::reactive({
      foundr::timetraitsall(traitSignal)
    })
    
    output$sideInput <- shiny::renderUI({
      shiny::req(input$tabpanel)
      
      if(shiny::isTruthy(entrykey())) {
        # Tab-specific side panel.
        shiny::req(input$tabpanel)
        if(input$tabpanel != "About") {
          shiny::tagList(
            shiny::fluidRow(
              shiny::column(3, mainParInput(ns("main_par"))), # dataset
              if(input$tabpanel %in% c("Traits", "Times"))
                shiny::column(3, mainParUI(ns("main_par"))), # order
              if(input$tabpanel %in% c("Traits","Times","Contrasts")) {
                shiny::column(6, 
                              switch(input$tabpanel,
                                     Traits    = traitInput(ns("tabTraits")),
                                     Contrasts = contrastInput(ns("tabContrasts")),
                                     Times     = if(length(timetraits_all()))
                                       timeInput(ns("tabTimes"))))
              }),
            
            switch(input$tabpanel,
                   Traits    = traitUI(ns("tabTraits")),
                   Contrasts = if(length(timetraits_all()))
                     contrastUI(ns("tabContrasts")),
                   Times     = if(length(timetraits_all())) timeUI(ns("tabTimes"))),
            
            border_line(),
            mainParOutput(ns("main_par")), # plot_table, height
            downloadOutput(ns("download"))
          )
        }
      }
    })
    # Don't show Entry Key if there is no need.
    output$entrykey <- shiny::renderUI({
      if(shiny::isTruthy(customSettings$entrykey))
        shiny::textInput(ns("appEntry"), "Entry Key:")
    })
    
    # Main Output
    output$mainOutput <- shiny::renderUI({
      if(shiny::isTruthy(entrykey())) {
        shiny::tabsetPanel(
          type = "tabs", header = "", id = ns("tabpanel"),
          shiny::tabPanel("Traits",    traitOutput(ns("tabTraits"))),
          shiny::tabPanel("Contrasts", contrastOutput(ns("tabContrasts"))),
          shiny::tabPanel("Stats",     statsOutput(ns("tabStats"))),
          shiny::tabPanel("Times",     timeOutput(ns("tabTimes"))),
          shiny::tabPanel("About",     aboutOutput(ns("about")))
        )
      }
    })
  })
}
#' @export
#' @rdname foundrServer
foundrInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("sideInput")),
    shiny::uiOutput(ns("entrykey")))
}
#' @export
#' @rdname foundrServer
foundrOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("mainOutput"))
}
#' @param title title of app
#' @export
#' @rdname foundrServer
foundrApp <- function(title = "") {
  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
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
