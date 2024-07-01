#' Shiny Module Server for Times Plots
#' @param id identifier for shiny reactive
#' @param panel_par,main_par reactive arguments 
#' @param traitSignal static object
#' @param traitOrder reactive object
#' @param responses possible types of responses
#'
#' @return nothing returned
#' @importFrom shiny column fluidRow h3 observeEvent moduleServer NS plotOutput
#'             radioButtons reactive reactiveValues renderPlot renderUI req
#'             selectInput selectizeInput tagList uiOutput updateSelectizeInput
#' @importFrom DT renderDataTable
#' @importFrom foundr timetraits timetraitsall
#' @export
#'
timeTraitsServer <- function(id, panel_par, main_par,
                            traitSignal, traitOrder,
                            responses = c("value", "normed", "cellmean")) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Identify all Time Traits.
    timetrait_all <- foundr::timetraitsall(traitSignal)
    
    # Inputs
    output$time_units <- shiny::renderUI({
      timeunits <- time_units(timetrait_all)
      
      shiny::selectInput(ns("time_units"), "Time Unit:", timeunits,
                         selections$time)
    })
    output$response <- shiny::renderUI({
      shiny::radioButtons(ns("response"), "Response:",
                          responses, selections$response, inline = TRUE)
    })
    selections <- shiny::reactiveValues(time = NULL, response = "cellmean",
                                        traits = NULL)
    shiny::observeEvent(input$time_units, selections$time <- input$time_units)
    shiny::observeEvent(input$response,   selections$response <- input$response)
    shiny::observeEvent(input$traits,     selections$traits <- input$traits)
    
    # Update `input$time_units` choices and selected.
    shiny::observeEvent(
      shiny::req(traitOrder()),
      {
        selected <- selections$time
        choices <- time_units(timetrait_order())
        selected <- selected[selected %in% choices]
        if(!length(selected)) selected <- choices[1]
        shiny::updateSelectInput(session, "time",
                                 choices = choices, selected = selected)
      }
    )
    
    # Update Trait choices and selected.
    shiny::observeEvent(
      shiny::tagList(selections$response, selections$time, traitOrder(),
                     trait_names(), main_par$tabpanel),
      {
        # Use current selection of trait_selection().
        # But make sure they are still in the traitOrder() object.
        selected <- selections$traits
        choices <- shiny::req(trait_names())
        selected <- selected[selected %in% choices]
        if(!length(selected))
          selected <- choices[1]
        shiny::updateSelectizeInput(session, "traits", choices = choices,
                                    server = TRUE, selected = selected)
        selections$traits <- selected
      })
    
    # Trait Order Criterion.
    timetrait_order <- shiny::reactive({
      out <- timetrait_all
      
      if(shiny::isTruthy(traitOrder())) {
        out <- dplyr::filter(
          dplyr::left_join(
            dplyr::select(traitOrder(), .data$dataset, .data$trait),
            out,
            by = c("dataset", "trait")),
          !is.na(timetrait))
      }
      out
    }, label = "timetrait_order")
    
    # Trait names (removing key time information).
    trait_names <- shiny::reactive({
      shiny::req(selections$time)
      if(shiny::isTruthy(main_par$tabpanel)) {
        shiny::req(main_par$tabpanel)
      }
      
      # Make sure timeunit aligns with trait names.
      object <- shiny::req(timetrait_order())
      timeunit <- selections$time
      if(!(timeunit %in% object$timetrait))
        timeunit <- sort(unique(object$timetrait))[1]
      
      foundr::timetraits(object, timeunit)
    }, label = "trait_names")
    
    ###############################################################
    selections
  })
}
#' Shiny Module Input for Time Traits
#' @return nothing returned
#' @rdname timeTraitsServer
#' @export
timeTraitsInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::selectizeInput(ns("traits"), "Traits:", NULL, multiple = TRUE) # traits
}
#' Shiny Module UI for Time Traits
#' @return nothing returned
#' @rdname timeTraitsServer
#' @export
timeTraitsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("time_units")) # time_units
}
#' Shiny Module Output for Time Traits
#' @return nothing returned
#' @rdname timeTraitsServer
#' @export
timeTraitsOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("response")) # response
}
#' Shiny Module App for Times Plot
#' @return nothing returned
#' @rdname timeTraitsServer
#' @export
timeTraitsApp <- function() {
  title <- "Test shiny Time Traits"
  
  ui <- function() {
    shiny::fluidPage(
      shiny::titlePanel(title),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::fluidRow(
            shiny::column(3, mainParInput("main_par")), # dataset
            shiny::column(9, timeTraitsInput("time_traits"))), # traits
          timeTraitsUI("time_traits"),
          timeTraitsOutput("time_traits") # response
        ),
        shiny::mainPanel(
          panelParInput("panel_par"), # strains, facet
          shiny::h4("Time Traits:"),
          shiny::textOutput("selections")
        )
      )
    )
  }
  
  server <- function(input, output, session) {
    # MODULES
    main_par <- mainParServer("main_par", traitStats)
    panel_par <- panelParServer("panel_par", main_par, traitStats)
    stats_table <- traitOrderServer("stats_table", main_par,
      time_trait_table, customSettings)
    # Subset Stats to time traits.
    time_trait_table <- time_trait_subset(traitStats,
      foundr::timetraitsall(traitSignal))
    # Identify Time Traits.
    time_traits <- timeTraitsServer("time_traits",
      panel_par, main_par, traitSignal, stats_table)
    
    output$selections <- shiny::renderText({
      shiny::req(time_traits$traits)
      paste(time_traits$traits, collapse = ", ")
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
