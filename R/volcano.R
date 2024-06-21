#' Shiny Module Server for Contrast Plots
#'
#' @param id identifier
#' @param panel_par,main_par input parameters
#' @param contrastTable reactive data frame
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
volcanoServer <- function(id, panel_par, main_par,
                              contrastTable, customSettings = NULL,
                              modTitle = shiny::reactive("Eigentrait Contrasts")) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # INPUTS
    # RETURNS
    
    # Input
    output$ordername <- shiny::renderUI({
      orders <- c("p.value","kME","size","module")
      orders <- orders[!is.na(match(orders, names(contrastTable())))]
      
      shiny::selectInput(ns("ordername"), "Order by:", orders)
    })
    ord_selection <- shiny::reactiveVal(NULL, label = "ord_selection")
    shiny::observeEvent(input$ordername, ord_selection(input$ordername))

    # Volcano paremeters    
    vol <- shiny::reactive({ vol_default(shiny::req(ord_selection())) },
                           label = "vol")
    
    output$volvert <- shiny::renderUI({
      shiny::req(vol())
      
      shiny::sliderInput(ns("volvert"),
                         paste(vol()$label, "line:"),
                         min = vol()$min, max = vol()$max,
                         value = vol()$value, step = vol()$step)
    })
    vert_selection <- shiny::reactiveVal(NULL, label = "vert_selection")
    shiny::observeEvent(input$volvert, vert_selection(input$volvert))
    
    output$rownames <- shiny::renderUI({
      title <- shiny::req(info())$title
      if(title == "Strains") {
        choices <- names(foundr::CCcolors)
      } else {
        choices <- term_stats(contrastTable(), signal = FALSE, drop_noise = TRUE)
      }
      shiny::checkboxGroupInput(ns("rownames"), "",
        choices = choices, selected = choices, inline = TRUE)
    })
    row_selection <- shiny::reactiveVal(NULL, label = "row_selection")
    shiny::observeEvent(input$rownames, row_selection(input$rownames))
    
    # Output
    output$traitOutput <- shiny::renderUI({
      switch(shiny::req(main_par$butshow),
             Plots  = shiny::uiOutput(ns("plot")), 
             Tables = DT::renderDataTable(tableObject(), escape = FALSE,
               options = list(scrollX = TRUE, pageLength = 10)))
    })
    
    # Plot
    info <- shiny::reactive({
      # Set up particulars for contrast or stat
      if(inherits(shiny::req(contrastTable()), "conditionContrasts"))
        list(row = "strain", col = "value", title = "Strains")
      else
        list(row = "term", col = "SD", title = "Terms")
    })
    # Filter to desired rownames (strains or terms).
    filter_rownames <- shiny::reactive({
      shiny::req(contrastTable(), row_selection(), info())
      
      dplyr::filter(contrastTable(), .data[[info()$row]] %in% row_selection())
    })
    
    # Threshold for Volcano plots
    threshold <- shiny::reactive({
      shiny::req(vert_selection(), input$volsd, ord_selection())
      
      out <- c(SD = input$volsd,
               p.value = 0.01, kME = 0.8, module = 10, size = 15)
      if(ord_selection() == "p.value")
        out[ord_selection()] <- 10 ^ -vert_selection()
      else
        out[ord_selection()] <- vert_selection()
      out
    })
    
    contrastVolcano <- shiny::reactive({
      shiny::req(filter_rownames())
      # Generic plot function for `traits` and `eigens`.``
      foundr::ggplot_conditionContrasts(
        filter_rownames(), bysex = panel_par$sex,
        ordername = ord_selection(),
        plottype = "volcano", threshold = threshold(),
        strain = input$strain,
        interact = shiny::isTruthy(input$interact))
    }, label = "contrastVolcano")
    
    output$plot <- shiny::renderUI({
      shiny::tagList(
        shiny::h4("Volcano Plot"),
        shiny::uiOutput(ns("rownames")),
        if(shiny::isTruthy(input$interact)) {
          plotly::renderPlotly(shiny::req(contrastVolcano()))
        } else {
          shiny::renderPlot(print(shiny::req(contrastVolcano())))
        }
      )
    })
    shiny::observeEvent(
      shiny::req(contrastTable(), ord_selection(), vol(), info()),
      {
        maxsd <- min(signif(max(abs(contrastTable()[[info()$col]]), na.rm = TRUE), 2), 5)
        shiny::updateSliderInput(session, "volsd", max = maxsd)
        
        if(ord_selection() == "p.value") {
          maxvert <- min(10, round(-log10(min(contrastTable()$p.value, na.rm = TRUE)), 1))
        } else {
          maxvert <- vol()$max
        }
        shiny::updateSliderInput(session, "volvert", max = maxvert)
      }, label = "observeSlider")

    ###############################################################
    contrastVolcano
  })
}
#' Shiny Module Input for Volcano Plots
#' @return nothing returned
#' @rdname volcanoServer
#' @export
volcanoInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(4, shiny::uiOutput(ns("ordername"))),
    shiny::column(8, shiny::checkboxInput(ns("interact"), "Interactive?")))
}
#' Shiny Module Output for Contrast Plots
#' @return nothing returned
#' @rdname volcanoServer
#' @export
volcanoOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Sliders from Volcano plot display.
    shiny::fluidRow(
      shiny::column(6, shiny::sliderInput(ns("volsd"),
        "SD line:", min = 0, max = 2, value = 1, step = 0.1)),
      shiny::column(6, shiny::uiOutput(ns("volvert")))),
    shiny::uiOutput(ns("title")),
    shiny::uiOutput(ns("traitOutput"))
  )
}
#' Shiny App for Volcano Plots
#' @return nothing returned
#' @rdname volcanoServer
#' @export
volcanoApp <- function() {
}
