#' Shiny Module Server for DotPlots
#'
#' @param id identifier
#' @param panel_par,plot_par input parameters
#' @param contrast_table reactive data frame
#'
#' @return reactive object 
#' @importFrom shiny column fluidRow moduleServer NS observeEvent
#'             radioButtons reactive reactiveVal reactiveValues renderUI
#'             req selectInput tagList uiOutput updateSelectInput
#' @importFrom DT renderDataTable
#' @importFrom foundr ggplot_conditionContrasts summary_conditionContrasts
#'             summary_strainstats
#' @export
#'
dotplotServer <- function(id, panel_par, plot_par, contrast_table) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Plot
    info <- shiny::reactive({
      # Set up particulars for contrast or stat
      if(inherits(shiny::req(contrast_table()), "conditionContrasts"))
        list(row = "strain", col = "value", title = "Strains")
      else
        list(row = "term", col = "SD", title = "Terms")
    })
    # Filter to desired rownames (strains or terms).
    filter_rownames <- shiny::reactive({
      shiny::req(contrast_table(), plot_par$rownames, info())
      
      dplyr::filter(contrast_table(), .data[[info()$row]] %in% plot_par$rownames)
    })
    
    # Threshold for DotPlot plots
    threshold <- shiny::reactive({
      shiny::req(plot_par$volvert, plot_par$volsd, plot_par$ordername)
      
      out <- c(SD = plot_par$volsd,
               p.value = 0.01, kME = 0.8, module = 10, size = 15)
      if(plot_par$ordername == "p.value")
        out[plot_par$ordername] <- 10 ^ -plot_par$volvert
      else
        out[plot_par$ordername] <- plot_par$volvert
      out
    })
    
    contrastDotPlot <- shiny::reactive({
      shiny::req(filter_rownames())
      # Generic plot function for `traits` and `eigens`.``
      foundr::ggplot_conditionContrasts(
        filter_rownames(), bysex = panel_par$sex,
        ntrait = input$ntrait,
        ordername = plot_par$ordername,
        plottype = "dotplot", threshold = threshold(),
        interact = shiny::isTruthy(plot_par$interact))
    }, label = "contrastDotPlot")
    
    output$plot <- shiny::renderUI({
      shiny::tagList(
        shiny::h4("DotPlot Plot"),
        shiny::uiOutput(ns("rownames")),
        shiny::numericInput(ns("ntrait"), "Traits:", 20, 5, 100, 5),
        if(shiny::isTruthy(plot_par$interact)) {
          plotly::renderPlotly(shiny::req(contrastDotPlot()))
        } else {
          shiny::renderPlot(print(shiny::req(contrastDotPlot())))
        }
      )
    })

    ###############################################################
    contrastDotPlot
  })
}
#' Shiny Module Output for Contrast Plots
#' @return nothing returned
#' @rdname dotplotServer
#' @export
dotplotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("title")),
    shiny::uiOutput(ns("plot"))
  )
}
#' Shiny App for DotPlot Plots
#' @return nothing returned
#' @rdname dotplotServer
#' @export
dotplotApp <- function() {
  title <- "Shiny DotPlot"
  ui <- function() {
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          mainParInput("main_par"), # dataset
          plotParInput("plot_par") # ordername, interact
        ),
        shiny::mainPanel(
          mainParOutput("main_par"), # plot_table, height
          plotParUI("plot_par"), # volsd, volvert (sliders)
          plotParOutput("plot_par"), # rownames (strains/terms)
          panelParUI("panel_par"), # sex
          dotplotOutput("dotplot")
        )
      )
    )
  }
  server <- function(input, output, session) {
    main_par <- mainParServer("main_par", traitStats)
    panel_par <- panelParServer("panel_par", main_par, traitStats)
    contrast_table <- contrastTableServer("contrast_table", main_par,
      traitSignal, traitStats, customSettings)
    plot_par <- plotParServer("plot_par", contrast_table)
    dotplotServer("dotplot", panel_par, plot_par, contrast_table)
  }
  shiny::shinyApp(ui, server)
}
