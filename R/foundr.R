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
    
    # INPUTS
    #    input$dataset
    #    input$height
    #    input$tabpanel
    
    # CALL MODULES
    traitServer("tabTraits", input, traitData, traitSignal, traitStats,
                    customSettings)
    timeServer("tabTimes", input, traitData, traitSignal, traitStats)
    statsServer("tabStats", input, traitStats, customSettings)
    contrastServer("tabContrasts", input,
                       traitSignal, traitStats, traitModule, customSettings)
    
    output$intro <- foundr:::foundrIntro(customSettings$help)
    
    # SERVER-SIDE INPUTS
    output$dataset <- shiny::renderUI({
      # Dataset selection.
      datasets <- unique(traitStats$dataset)
      selected <- data_selection()
      
      # Get datasets.
      shiny::selectInput(ns("dataset"), "Datasets:", datasets, selected,
                         multiple = TRUE)
    })
    data_selection <- shiny::reactiveVal(unique(traitStats$dataset)[1],
                                         label = "data_selection")
    shiny::observeEvent(input$dataset, data_selection(input$dataset))
    
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
      shiny::tagList(input$height, entrykey()),
      {
        if(shiny::isTruthy(entrykey())) {
          shiny::showTab("tabpanel", target = "Traits")
          if(length(timetraits_all()))
            shiny::showTab("tabpanel", target = "Times")
          shiny::showTab("tabpanel", target = "Contrasts")
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
    # Hide Time tab unless we have time entries.
    shiny::observeEvent(
      input$height,
      {
        if(shiny::isTruthy(entrykey())) {
          if(length(timetraits_all())) {
            shiny::showTab("tabpanel", target = "Times")
            # Hidden for calcium study for now.
            shiny::showTab("tabpanel", target = "Contrasts")
          } else {
            shiny::hideTab("tabpanel", target = "Times")
            # Hidden for calcium study for now.
            shiny::hideTab("tabpanel", target = "Contrasts")
          }}
      })
    timetraits_all <- shiny::reactive({
      foundr::timetraitsall(traitSignal)
    })
    
    output$sideInput <- shiny::renderUI({
      shiny::req(input$tabpanel)
      
      if(shiny::isTruthy(entrykey())) {
        # Tab-specific side panel.
        shiny::req(input$tabpanel)
        shiny::tagList(
          shiny::fluidRow(
            shiny::column(3, shiny::uiOutput(ns("dataset"))),
            if(input$tabpanel %in% c("Traits","Times","Contrasts")) {
              shiny::column(9, 
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
          
          shiny::hr(style="border-width:5px;color:black;background-color:black"),
          
          shiny::sliderInput(ns("height"), "Plot height (in):", 3, 10, 6,
                             step = 1))
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
          shiny::tabPanel("About",     shiny::uiOutput(ns("intro")))
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
  server <- function(input, output, server) {
    foundrServer("foundr",
                 traitData, traitSignal, traitStats,
                 customSettings, traitModule)
  }
  shiny::shinyApp(ui, server)
}
