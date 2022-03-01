source("shiny-app/FactorSurvivalPlot.R")
source("shiny-app/FactorImplantSurvival.R")

factorSurvivalPlotBasicInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("selectFactor")),
    htmlOutput(ns("selectLevels")),
    sliderTextInput(
      inputId = ns("selectFirst"),
      label = "Choose first year:", 
      choices = c(1, 2, 3, 4, 5),
      grid = TRUE,
      selected = 2
    ),
    sliderTextInput(
      inputId = ns("selectSecond"),
      label = "Choose second year:",
      choices = c(4, 5, 6, 7, 8, 9),
      grid = TRUE,
      selected = 5
    )
  )
}

factorImplantSurvivalPlotInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("selectLotNrImplants"))
    )
}

factorSurvivalPlotGeneralInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionBttn(ns("add"), "Add to report", style = "bordered", color = "warning")
  )
}

factorSurvivalPlotUI <- function(id){
  ns <- NS(id)
  plotOutput(ns("factorSurvivalPlot"), height = 650) %>% withSpinner()
}

factorImplantSurvivalPlotUI <- function(id){
  ns <- NS(id)
  plotOutput(ns("factorImplantSurvivalPlot"), height = 500) %>% withSpinner()
}

factorSurvivalServer <- function(id, data, plotInReport, overallFilter, overallFilterLevels) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(input$add, {
        # Show a modal when the button is pressed
        shinyalert::shinyalert(
          title = "Confirm",
          text = "Do you want to add this graph to the report?",
          size = "s", 
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "warning",
          showConfirmButton = TRUE,
          showCancelButton = TRUE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          cancelButtonText = "Cancel",
          timer = 0,
          imageUrl = "",
          animation = TRUE,
          inputId = "confirm"
        )
      })
      
      observeEvent(input$confirm, {
        if(input$confirm){
          plotInReport$dList <- c(isolate(plotInReport$dList),
                                  list(c("factor" = isolate(input$selectFactor),
                                         "levels" = isolate(paste(input$selectLevels, collapse = ";")),
                                         "firstYear" = isolate(input$selectFirst),
                                         "secondYear" = isolate(input$selectSecond),
                                         "selectedLotNrImplants" = isolate(paste(input$selectLotNrImplants, collapse = ";")),
                                         "overallFilter" = isolate(overallFilter()),
                                         "overallFilterLevels" = isolate(paste(overallFilterLevels(), collapse = ";")),
                                         "tab" = "Implant Survival")))
        }
      })
      
      ## Inputs -------------------------------------------------------------------
      
      output$selectFactor <- renderUI({
        pickerInput(ns("selectFactor"),
                    "Select Factor",
                    choices = c(data %>% select_if( ~ is.factor(.)) %>% names(), "None"),
                    selected = "None"
        )
      })
      
      output$selectLevels <- renderUI({
        req(isTruthy(input$selectFactor) & isTRUE(input$selectFactor != "None"))
        pickerInput(ns("selectLevels"),
                    paste0("Filter ", input$selectFactor),
                    choices = as.character(
                      sort(unique(data[[input$selectFactor]]))
                    ),
                    multiple = T,
                    options = list(
                      `actions-box` = TRUE, size = 10,
                      `selected-text-format` = "count > 2"
                    )
        )
      })
      
      output$selectLotNrImplants <- renderUI({
        pickerInput(ns("selectLotNrImplants"),
                    "Select Implant(s)",
                    choices = as.character(sort(unique(data[["ImplantName"]]))),
                    multiple = T,
                    options = list(
                      `actions-box` = TRUE, size = 10,
                      `selected-text-format` = "count > 2"
                    )
        )
      })
      
      ## Plots --------------------------------------------------------------------
      
      
      output$factorSurvivalPlot <- renderPlot({
        req(isTruthy(input$selectFirst))
        req(isTruthy(input$selectSecond))
        req(isTruthy(input$selectFactor))
        
        factorSurvivalPlot(data = data,
                            factor = input$selectFactor,
                            levels = input$selectLevels,
                            firstYear = input$selectFirst,
                            secondYear = input$selectSecond,
                            overallFilter = overallFilter(),
                            overallFilterLevels = overallFilterLevels())
      })
      
      output$factorImplantSurvivalPlot <- renderPlot({
        req(isTruthy(input$selectFirst))
        req(isTruthy(input$selectSecond))
        req(isTruthy(input$selectFactor))
        
        factorImplantSurvivalPlot(data = data,
                                 factor = input$selectFactor,
                                 levels = input$selectLevels,
                                 selectedImplants = input$selectLotNrImplants,
                                 firstYear = input$selectFirst,
                                 secondYear = input$selectSecond,
                                 overallFilter = overallFilter(),
                                 overallFilterLevels = overallFilterLevels())
      })
      
    }
  )
}

