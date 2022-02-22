source("shiny-app/ExplorerPlot.R")

explorePlotOptionsUI <- function(id) {
  ns <- NS(id)
  tagList(
    radioGroupButtons(
      inputId = ns("insertionsOrRemovals"),
      label = "Choose dataset",
      choices = c("Insertions", 
                  "Removals"),
      justified = TRUE,
      status = "primary"
    ),
    htmlOutput(ns("y"), width = 12),
    htmlOutput(ns("x"), width = 12),
    htmlOutput(ns("xLevels"), width = 12),
    htmlOutput(ns("factor1"), width = 12),
    htmlOutput(ns("factor1Levels"), width = 12),
    htmlOutput(ns("factor2"), width = 12),
    htmlOutput(ns("factor2Levels"), width = 12),
    htmlOutput(ns("factorColor"), width = 12),
    htmlOutput(ns("factorColorLevels"), width = 12),
    actionBttn(ns("add"), "Add to report", style = "bordered", color = "warning")
  )
}

explorerPlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot"), height = 500) %>% withSpinner()
  )
}

explorerPlotServer <- function(id,
                               data,
                               plotInReport) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      factorOptions <- data %>%
        select(where(is.factor) & -ends_with("Id")) %>%
        names()
      logicalOptions <- data %>%
        select(where(is.logical) & -ends_with("Id")) %>%
        names()
      numericOptions <- data %>%
        select(where(is.numeric) & -ends_with("Id")) %>%
        names()
      
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
            animation = TRUE
          )
        })
        
      observeEvent(input$shinyalert, {
        if(input$shinyalert){
          plotInReport$dList <- c(isolate(plotInReport$dList),
                              list(c("y" = isolate(input$y),
                                     "x" = isolate(input$x),
                                     "factor1" = isolate(input$factor1),
                                     "factor1Levels" = isolate(paste(input$factor1Levels, collapse = ";")),
                                     "factor2" = isolate(input$factor2),
                                     "factor2Levels" = isolate(paste(input$factor2Levels, collapse = ";")),
                                     "factorColor" = isolate(input$factorColor),
                                     "factorColorLevels" = isolate(paste(input$factorColorLevels, collapse = ";")),
                                     "insertionsOrRemovals" =  isolate(input$insertionsOrRemovals),
                                     "tab" = "Explorer")))
        }
      })

      ### Y-axis
      output$y <- renderUI({
        pickerInput(ns("y"),
          "Select Y-axis",
          choices = c(numericOptions, logicalOptions),
          selected = "Complications"
        )
      })

      ### X-axis
      output$x <- renderUI({
        pickerInput(ns("x"),
          "Select X-axis",
          choices = list(
            Factors = factorOptions,
            Numeric = numericOptions,
            Logical =logicalOptions)
        )
      })
      
      output$xLevels <- renderUI({
        req(input$x)
        
        if (!is.numeric(data[[input$x]])) {
          pickerInput(ns("xLevels"),
                      label = paste0("Filter ", input$x),
                      choices = as.character(
                        sort(unique(data[[input$x]]))
                      ),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE, size = 10,
                        `selected-text-format` = "count > 2"
                      )
          )
        }
      })

      ### Fill color
      output$factorColor <- renderUI({
          pickerInput(ns("factorColor"),
            "Select Fill Color",
            choices = list(
              None = c("None"),
              Factor = factorOptions,
              Logical = logicalOptions),
            selected = "None"
          )
      })
      
      output$factorColorLevels <- renderUI({
        req(input$factorColor)
        req(input$factorColor != "None")
        
        pickerInput(ns("factorColorLevels"),
                    label = paste0("Filter ", input$factorColor),
                    choices = as.character(
                      sort(unique(data[[input$factorColor]]))
                    ),
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE, size = 10,
                      `selected-text-format` = "count > 2"
                    )
        )
      })

      ### Facet row
      output$factor1 <- renderUI({
        pickerInput(ns("factor1"),
          "Select Facet Row",
          choices = list(
            None = c("None"),
            Factor = factorOptions,
            Logical = logicalOptions),
          selected = "None"
        )
      })

      ### Specific facet row
      output$factor1Levels <- renderUI({
        req(input$factor1)
        req(input$factor1 != "None")

          pickerInput(ns("factor1Levels"),
            label = paste0("Filter ", input$factor1),
            choices = as.character(
              sort(unique(data[[input$factor1]]))
            ),
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE, size = 10,
              `selected-text-format` = "count > 2"
            )
          )
      })
      
      ### Facet row
      output$factor2 <- renderUI({
        pickerInput(ns("factor2"),
                    "Select Facet Row",
                    choices = list(
                      None = c("None"),
                      Factor = factorOptions,
                      Logical = logicalOptions),
                    selected = "None"
        )
      })
      
      ### Specific facet row
      output$factor2Levels <- renderUI({
        req(input$factor2)
        req(input$factor2 != "None")
        
        pickerInput(ns("factor2Levels"),
                    label = paste0("Filter ", input$factor2),
                    choices = as.character(
                      sort(unique(data[[input$factor2]]))
                    ),
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE, size = 10,
                      `selected-text-format` = "count > 2"
                    )
        )
      })

      ## Plots -------------------------------------------------------------------

      output$plot <- renderPlot({
        req(
          isTruthy(data),
          isTruthy(input$y),
          isTruthy(input$x)
        )
        
        explorerPlot(
          data = data,
          y = input$y,
          x = input$x,
          xLevels = input$xLevels,
          factor1 = input$factor1,
          factor1Levels = input$factor1Levels,
          factor2 = input$factor2,
          factor2Levels = input$factor2Levels,
          factorColor = input$factorColor,
          factorColorLevels = input$factorColorLevels,
          InsertionsOrRemovals = input$insertionsOrRemovals
        )
      })
    }
  )
}

