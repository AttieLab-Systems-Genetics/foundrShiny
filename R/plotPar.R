#' Shiny Module Server for Plot Parameters
#'
#' @param id identifier
#' @param contrastTable reactive data frame
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
plotParServer <- function(id, contrastTable) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Parameters
    # ordername
    # volvert
    # volsd
    # rownames
    # interact

    # Input ordername
    output$ordername <- shiny::renderUI({
      shiny::req(contrastTable())
      orders <- c("p.value","kME","size","module")
      orders <- orders[!is.na(match(orders, names(contrastTable())))]
      
      shiny::selectInput(ns("ordername"), "Order by:", orders)
    })
    ord_selection <- shiny::reactiveVal(NULL, label = "ord_selection")
    shiny::observeEvent(input$ordername, ord_selection(input$ordername))
    
    # Input volvert
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
    
    # Input rownames
    info <- shiny::reactive({
      # Set up particulars for contrast or stat
      if(inherits(shiny::req(contrastTable()), "conditionContrasts"))
        list(row = "strain", col = "value", title = "Strains")
      else
        list(row = "term", col = "SD", title = "Terms")
    })
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


    ###############################################################
    input
  })
}
#' Shiny Module Input for Plot Parameters
#' @return nothing returned
#' @rdname plotParServer
#' @export
plotParInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(4, shiny::uiOutput(ns("ordername"))),
    shiny::column(8, shiny::checkboxInput(ns("interact"), "Interactive?")))
}
#' Shiny Module UI for Plot Parameters
#' @return nothing returned
#' @rdname plotParServer
#' @export
plotParUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Sliders from Volcano plot display.
    shiny::fluidRow(
      shiny::column(6, shiny::sliderInput(ns("volsd"),
        "SD line:", min = 0, max = 2, value = 1, step = 0.1)),
      shiny::column(6, shiny::uiOutput(ns("volvert")))),
    shiny::uiOutput(ns("rownames")))
}
#' Shiny Module App for Plot Parameters
#' @return nothing returned
#' @rdname plotParServer
#' @export
plotParApp <- function() {
  ui <- shiny::bootstrapPage(
    mainParInput("main_par"),
    shiny::hr(style="border-width:5px;color:black;background-color:black"),
    plotParInput("plot_par"), 
    plotParUI("plot_par")
  )
  server <- function(input, output, session) {
    main_par <- mainParServer("main_par", traitStats)
    contrast_table <- contrastTableServer("contrast_table", main_par,
      traitSignal, traitStats, customSettings)
    plotParServer("plot_par", contrast_table)
  }
  shiny::shinyApp(ui, server)
}
