#' Shiny Module Server for Contrast Plots
#'
#' @param id identifier
#' @param panel_par,main_par input parameters
#' @param contrast_table reactive data frame
#' @param customSettings list of custom settings
#' @param modTitle character string title for section
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
contrastPlotServer <- function(id, panel_par, main_par,
                              contrast_table, customSettings = NULL,
                              modTitle = shiny::reactive("Contrasts")) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    plot_par <- plotParServer("plot_par", contrast_table)
    
    output$traitOutput <- shiny::renderUI({
      shiny::tagList(
        shiny::h3(modTitle()),
        switch(shiny::req(main_par$butshow),
          Plots  = shiny::tagList(
            shiny::uiOutput(ns("plots")),
            shiny::uiOutput(ns("plot"))), 
          Tables = DT::renderDataTable(tableObject(), escape = FALSE,
            options = list(scrollX = TRUE, pageLength = 10))))
    })
    
    # Filter to desired strains.
    contrasts_strains <- shiny::reactive({
      shiny::req(contrast_table(), plot_par$rownames)
      datarow <- ifelse(inherits(contrast_table(), "conditionContrasts"),
                        "strain", "term")
      dplyr::filter(contrast_table(),
                    .data[[datarow]] %in% plot_par$rownames)
    })
    
    # Generic plot function for `traits` and `eigens`.``
    plotfn <- function(data, plottype) {
      foundr::ggplot_conditionContrasts(
        data, bysex = panel_par$sex,
        ntrait = input$ntrait,
        ordername = plot_par$ordername,
        plottype = plottype, threshold = threshold(),
        strain = input$strain,
        interact = shiny::isTruthy(plot_par$interact))
    }
    threshold <- shiny::reactive({
      shiny::req(plot_par$volvert, plot_par$volsd, plot_par$ordername)
      out <- c(SD=plot_par$volsd, p.value=0.01, kME=0.8, module=10, size=15)
      if(plot_par$ordername == "p.value")
        out[plot_par$ordername] <- 10 ^ -plot_par$volvert
      else
        out[plot_par$ordername] <- plot_par$volvert
      out
    })
    
    contrastVolcano <- shiny::reactive({
      shiny::req(contrasts_strains())
      plotfn(contrasts_strains(), "volcano")
    }, label = "contrastVolcano")
    contrastBiPlot <- shiny::reactive({
      shiny::req(contrasts_strains())
      plotfn(contrasts_strains(), "biplot")
    }, label = "contrastBiPlot")
    contrastDotPlot <- shiny::reactive({
      shiny::req(contrasts_strains(), input$ntrait)
      plotfn(contrasts_strains(), "dotplot")
    }, label = "contrastDotPlot")
    
    output$plots <- shiny::renderUI({
      choices <- c("Volcano","BiPlot","DotPlot")
      shiny::checkboxGroupInput(ns("plots"), "",
        choices = choices, selected = choices, inline = TRUE)
    })
    plot_selection <- shiny::reactiveVal(NULL, label = "plot_selection")
    shiny::observeEvent(input$plots, plot_selection(input$plots))
    output$plot <- shiny::renderUI({
      shiny::tagList(
        if("Volcano" %in% plot_selection()) {
          shiny::tagList(
            shiny::h4("Volcano Plot"),
            shiny::uiOutput(ns("convolcano")))
        },
        if("BiPlot" %in% plot_selection()) {
          shiny::tagList(
            shiny::h4("BiPlot"),
            shiny::selectInput(ns("strain"), "Vector Highlight",
                               c("NONE", plot_par$rownames)),
            shiny::uiOutput(ns("conbiplot")))
        },
        if("DotPlot" %in% plot_selection()) {
          shiny::tagList(
            shiny::h4("DotPlot"),
            shiny::uiOutput(ns("condotplot")))
        })
    })
    output$convolcano <- shiny::renderUI({
      if(shiny::isTruthy(plot_par$interact)) {
        plotly::renderPlotly(shiny::req(contrastVolcano()))
      } else {
        shiny::renderPlot(print(shiny::req(contrastVolcano())))
      }
    })
    output$conbiplot <- shiny::renderUI({
      if(shiny::isTruthy(plot_par$interact)) {
        shiny::tagList(
          shiny::renderText("Rays disappear if interactive."),
          shiny::renderPlot(print(shiny::req(contrastBiPlot()))),
          plotly::renderPlotly(shiny::req(contrastBiPlot())))
      } else {
        shiny::renderPlot(print(shiny::req(contrastBiPlot())))
      }
    })
    output$condotplot <- shiny::renderUI({
      shiny::tagList(
        shiny::numericInput(ns("ntrait"), "Traits:", 20, 5, 100, 5),
        if(shiny::isTruthy(plot_par$interact)) {
          plotly::renderPlotly(shiny::req(contrastDotPlot()))
        } else {
          shiny::renderPlot(print(shiny::req(contrastDotPlot())))
        })
    })
    
    tableObject <- shiny::reactive({
      shiny::req(contrast_table())
      title <- ifelse(inherits(contrast_table(), "conditionContrasts"),
                      "Strains", "Terms")
      if(title == "Strains") {
        foundr::summary_conditionContrasts(
          dplyr::filter(contrast_table(), sex == shiny::req(panel_par$sex)),
          ntrait = 0)
      } else { # title == "Terms"
        foundr::summary_strainstats(contrast_table(),
                            stats = "log10.p", model = "terms",
                            threshold = c(p.value = 1.0, SD = 0.0))
      }
    })
    
    ###############################################################
    shiny::reactiveValues(
      postfix = shiny::reactive({
        shiny::req(contrast_table())
        paste(unique(contrast_table()$dataset), collapse = ",")
      }),
      plotObject = shiny::reactive({
        if("Volcano" %in% plot_selection())
          print(shiny::req(contrastVolcano()))
        if("BiPlot" %in% plot_selection())
          print(shiny::req(contrastBiPlot()))
        if("DotPlot" %in% plot_selection())
          print(shiny::req(contrastDotPlot()))
      }),
      tableObject = tableObject)
  })
}
#' Shiny Module UI for Contrast Plots
#' @return nothing returned
#' @rdname contrastPlotServer
#' @export
contrastPlotUI <- function(id) {
  ns <- shiny::NS(id)
  plotParInput(ns("plot_par"))
}
#' Shiny Module Output for Contrast Plots
#' @return nothing returned
#' @rdname contrastPlotServer
#' @export
contrastPlotOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    plotParUI(ns("plot_par")),
    shiny::uiOutput(ns("traitOutput")))
}
#' Shiny Sex App for Contrast Plots
#'
#' @return nothing returned
#' @rdname contrastSexServer
#' @export
contrastPlotApp <- function() {
  title <- "Test contrastSex Module"
  
  ui <- function() {
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          mainParInput("main_par"),
          shiny::hr(style="border-width:5px;color:black;background-color:black"),
          mainParUI("main_par")
        ),
        shiny::mainPanel(
          shiny::fluidRow(
            shiny::column(4, shiny::uiOutput("sex")),
            shiny::column(8, contrastPlotUI("contrast_plot"))),
          contrastPlotOutput("contrast_plot")
        )
      )
    )
  }
  
  server <- function(input, output, session) {
    # Contrast Trait Table
    main_par <- mainParServer("main_par", traitStats)
    contrast_table <- contrastTableServer("contrast_table", main_par,
      traitSignal, traitStats, customSettings)
    contrastPlotServer("contrast_plot", input, main_par,
      contrast_table, customSettings)

    # SERVER-SIDE INPUTS
    output$strains <- shiny::renderUI({
      choices <- names(foundr::CCcolors)
      shiny::checkboxGroupInput(
        "strains", "Strains",
        choices = choices, selected = choices, inline = TRUE)
    })
    sexes <- c(B = "Both Sexes", F = "Female", M = "Male", C = "Sex Contrast")
    output$sex <- shiny::renderUI({
      shiny::selectInput("sex", "", as.vector(sexes))
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
