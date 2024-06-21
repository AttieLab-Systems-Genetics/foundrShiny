#' Shiny Module Server for BiPlots
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
biplotServer <- function(id, panel_par, plot_par, contrast_table) {
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
    
    # Threshold for BiPlot plots
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
    
    contrastBiPlot <- shiny::reactive({
      shiny::req(filter_rownames())
      # Generic plot function for `traits` and `eigens`.``
      foundr::ggplot_conditionContrasts(
        filter_rownames(), bysex = panel_par$sex,
        ordername = plot_par$ordername,
        plottype = "biplot", threshold = threshold(),
        strain = input$strain,
        interact = shiny::isTruthy(plot_par$interact))
    }, label = "contrastBiPlot")
    
    output$plot <- shiny::renderUI({
      shiny::tagList(
        shiny::h4("BiPlot Plot"),
        shiny::uiOutput(ns("rownames")),
        shiny::selectInput(ns("strain"), "Vector Highlight",
                           c("NONE", plot_par$rownames)),
        if(shiny::isTruthy(plot_par$interact)) {
          shiny::tagList(
            shiny::renderText("Rays disappear if interactive."),
            shiny::renderPlot(print(shiny::req(biplot()))),
            plotly::renderPlotly(shiny::req(contrastBiPlot())))
        } else {
          shiny::renderPlot(print(shiny::req(contrastBiPlot())))
        }
      )
    })

    ###############################################################
    contrastBiPlot
  })
}
#' Shiny Module Output for Contrast Plots
#' @return nothing returned
#' @rdname biplotServer
#' @export
biplotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("title")),
    shiny::uiOutput(ns("plot"))
  )
}
#' Shiny App for BiPlot Plots
#' @return nothing returned
#' @rdname biplotServer
#' @export
biplotApp <- function() {
  title <- "Shiny BiPlot"
  ui <- function() {
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          mainParInput("main_par"),
          plotParInput("plot_par")
        ),
        shiny::mainPanel(
          mainParOutput("main_par"),
          plotParUI("plot_par"),
          shiny::uiOutput("sex"),
          biplotOutput("biplot")
        )
      )
    )
  }
  server <- function(input, output, session) {
    main_par <- mainParServer("main_par", traitStats)
    contrast_table <- contrastTableServer("contrast_table", main_par,
      traitSignal, traitStats, customSettings)
    plot_par <- plotParServer("plot_par", contrast_table)
    biplotServer("biplot", input, plot_par, contrast_table)
    
    # SERVER-SIDE INPUTS
    sexes <- c(B = "Both Sexes", F = "Female", M = "Male", C = "Sex Contrast")
    output$sex <- shiny::renderUI({
      shiny::selectInput("sex", "", as.vector(sexes))
    })
  }
  shiny::shinyApp(ui, server)
}
