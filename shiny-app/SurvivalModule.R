source("shiny-app/SurvivalPlot.R")

survivalPlotBasicInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("selectFactor")),
    htmlOutput(ns("selectLevels")),
  )
}

survivalPlotAdvancedInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("selectAdditionalFactor")),
    htmlOutput(ns("selectAdditionalLevels")),
  )
}

survivalPlotGeneralInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionBttn(ns("add"), "Add to report", style = "bordered", color = "warning")
  )
}

survivalPlotUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("survivalPlot"), height = 600) %>% withSpinner()
}

survivalPlotServer <- function(id, data, plotInReport, overallFilter, overallFilterLevels) {
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
                                         "additionalFactor" = isolate(input$selectAdditionalFactor),
                                         "additionalLevels" = isolate(paste(input$selectAdditionalLevels, collapse = ";")),
                                         "overallFilter" = isolate(overallFilter()),
                                         "overallFilterLevels" = isolate(paste(overallFilterLevels(), collapse = ";")),
                                         "tab" = "Survival")))
        }
      })
      
      ## Inputs -------------------------------------------------------------------
      output$selectFactor <- renderUI({
        pickerInput(ns("selectFactor"),
                    "Select Factor",
                    choices = c(data %>% select_if( ~ is.factor(.) |  is.logical(.)) %>% names(), "None"),
                    selected = "None"
        )
      })
      
      output$selectLevels <- renderUI({
        req(isTruthy(input$selectFactor))
        req(isTRUE(input$selectFactor != "None"))
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
      
      output$selectAdditionalFactor <- renderUI({
        req(input$selectFactor)
        pickerInput(ns("selectAdditionalFactor"),
                    "Select Additional Factor",
                    choices = c(data %>% 
                      select_if( ~ (is.factor(.) |  is.logical(.))) %>%
                      names(), "None"),
                    selected = "None"
        )
      })
      
      output$selectAdditionalLevels <- renderUI({
        req(input$selectAdditionalFactors)
        req(input$selectAdditionalFactors != "None")
        pickerInput(ns("selectAdditionalLevels"),
                    paste0("Filter ", input$additionalFactor),
                    choices = as.character(
                      sort(unique(data[[input$additionalFactor]]))
                    ),
                    multiple = T,
                    options = list(
                      `actions-box` = TRUE, size = 10,
                      `selected-text-format` = "count > 2"
                    )
        )
      })
      
      ## Plots --------------------------------------------------------------------
      output$survivalPlot <- renderPlot({
        req(isTruthy(input$selectFactor))
        
        survivalPlot(data = data,
                     factor = input$selectFactor,
                     levels = input$selectLevels,
                     additionalFactor = input$selectAdditionalFactor,
                     additionalLevels = input$selectAdditionalLevels,
                     overallFilter = overallFilter(),
                     overallFilterLevels = overallFilterLevels())
      })
    }
  )
}
