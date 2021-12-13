source("shiny-app/ExplorerPlot.R")

explorePlotOptionsUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("selectYAxis"), width = 12),
    htmlOutput(ns("selectXAxis"), width = 12),
    htmlOutput(ns("selectFactorLevels"), width = 12),
    htmlOutput(ns("selectFillColor"), width = 12),
    htmlOutput(ns("selectFacetRow"), width = 12),
    htmlOutput(ns("selectSpecificFacetRow"), width = 12),
    checkboxGroupButtons(
      inputId = ns("graphOptions"),
      label = "Show",
      choices = c("x-lab", "Mean"),
      status = "info",
      selected = "x-lab"
    ),
    actionBttn(ns("add"), "Add to report", style = "bordered", color = "warning")
  )
}

explorerPlot <- function(id) {
  ns <- NS(id)
  tagList(
    h4(textOutput(ns("plotName"))),
    plotOutput(ns("plot"), height = 500) %>% withSpinner(id = ns("plotSpinner"))
  )
}

explorerPlotServer <- function(id,
                               data,
                               optionsToRemove,
                               allCombined,
                               selectedClinic,
                               clinicCompare,
                               isVisible,
                               plotInReport) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      factorOptions <- data %>%
        select(where(is.factor) & !any_of(optionsToRemove)) %>%
        names()
      logicalOptions <- data %>%
        select(where(is.logical) & !any_of(optionsToRemove)) %>%
        names()
      numericOptions <- data %>%
        select(where(is.numeric) & !any_of(optionsToRemove)) %>%
        names()

      observeEvent(isVisible(), {
        if (isVisible()) {
          shinyjs::show("plotName")
          shinyjs::show("plotSpinner")
          shinyjs::show("plot")
        } else {
          shinyjs::hide("plotName")
          shinyjs::hide("plotSpinner")
          shinyjs::hide("plot")
        }
      })

      observeEvent(input$selectXAxis,
        {
          if (isTruthy(input$selectXAxis)) {
            updateCheckboxGroupButtons(
              session,
              inputId = "graphOptions",
              disabledChoices = if (is.numeric(data[[input$selectXAxis]])) {
                c("Mean")
              } else {
                NULL
              }
            )
          }
        },
        ignoreInit = TRUE
      )
      
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
                              list(c("y" = isolate(input$selectYAxis),
                                     "x" = isolate(input$selectXAxis),
                                     "allCombined" = isolate(allCombined()),
                                     "selectedClinic" = isolate(selectedClinic()),
                                     "clinicCompare" = isolate(paste(clinicCompare(), collapse = ";")),
                                     "facetRow" =  isolate(input$selectFacetRow),
                                     "specificFacetRow" =  isolate(paste(input$selectSpecificFacetRow, collapese = ";")),
                                     "factorLevels" =  isolate(paste(input$selectFactorLevels, collapse = ";")),
                                     "fillColor" = isolate(input$selectFillColor),
                                     "dataset" = id,
                                     "tab" = "Explorer")))
        }
      })

      ### Y-axis
      output$selectYAxis <- renderUI({
        pickerInput(ns("selectYAxis"),
          "Select Y-axis",
          choices = c(numericOptions, logicalOptions, "Antall"),
          selected = "Complications"
        )
      })

      ### X-axis
      output$selectXAxis <- renderUI({
        pickerInput(ns("selectXAxis"),
          "Select X-axis",
          choices = c(factorOptions, numericOptions),
          selected = "Clinic"
        )
      })

      ### Fill color
      output$selectFillColor <- renderUI({
        if (allCombined()) {
          pickerInput(ns("selectFillColor"),
            "Select Fill Color",
            choices = c(factorOptions, logicalOptions, "None"),
            selected = "None"
          )
        }
      })

      ### X-axis levels
      output$selectFactorLevels <- renderUI({
        req(input$selectXAxis)

        if (is.factor(data[[input$selectXAxis]]) &
          (isTruthy(input$selectXAxis != "Clinic") | allCombined())) {
          pickerInput(ns("selectFactorLevels"),
            label = paste0("Select ", input$selectXAxis),
            choices = as.character(
              sort(unique(data[[input$selectXAxis]]))
            ),
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE, size = 10,
              `selected-text-format` = "count > 2"
            )
          )
        }
      })

      ### Facet row
      output$selectFacetRow <- renderUI({
        pickerInput(ns("selectFacetRow"),
          "Select Facet Row",
          choices = c(factorOptions, logicalOptions, "None"),
          selected = "None"
        )
      })

      ### Specific facet row
      output$selectSpecificFacetRow <- renderUI({
        req(input$selectFacetRow)

        if (isTRUE(input$selectFacetRow != "None")) {
          pickerInput(ns("selectSpecificFacetRow"),
            label = paste0("Select ", input$selectFacetRow),
            choices = as.character(
              sort(unique(data[[input$selectFacetRow]]))
            ),
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE, size = 10,
              `selected-text-format` = "count > 2"
            )
          )
        }
      })

      ## Plots -------------------------------------------------------------------

      # Render Insertions or removals depending on the plot viewed by user
      output$plotName <- renderText({
        paste0("Explore ", id)
      })

      output$plot <- renderPlot({
        req(
          isTruthy(data),
          isTruthy(input$selectYAxis),
          isTruthy(input$selectXAxis)
        )

        exploreDataPlot(
          data,
          "Mean" %in% input$graphOptions,
          !("x-lab" %in% input$graphOptions),
          selectedClinic(),
          clinicCompare(),
          if (allCombined()) input$selectFillColor else "Clinic",
          input$selectFacetRow,
          input$selectSpecificFacetRow,
          input$selectFactorLevels,
          input$selectYAxis,
          input$selectXAxis,
          allCombined()
        )
      })
    }
  )
}
