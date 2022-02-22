source("shiny-app/ImplantPlot.R")

implantInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("selectYears")),
    htmlOutput(ns("selectRemovalReason")),
    htmlOutput(ns("selectFactor")),
    htmlOutput(ns("selectLevels")),
    checkboxGroupButtons(
      inputId = ns("implantPlotOptions"),
      label = "Show",
      choices = c("Mean"),
      status = "info",
      selected = NULL
    ),
    actionBttn(ns("add"), "Add to report", style = "bordered", color = "warning")
  )
}

implantPlotUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("removalsImplantPlot"), height = 650) %>% withSpinner()
}

implantServer <- function(id, data, plotInReport) {
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
        if (input$confirm) {
          plotInReport$dList <- c(
            isolate(plotInReport$dList),
            list(c(
              "removalReasons" = isolate(paste(input$removalReasons, collapse = ";")),
              "years" = isolate(paste(input$selectYears, collapse = ";")),
              "factor" = isolate(input$selectFactor),
              "levels" = isolate(paste(input$selectLevels, collapse = ";")),
              "showMean" = isolate("Mean" %in% input$implantPlotOptions),
              "tab" = "Implant"
            ))
          )
        }
      })

      ## Inputs -------------------------------------------------------------------
      output$selectYears <- renderUI({
        pickerInput(ns("selectYears"),
          "Filter interval(s)",
          choices = as.character(sort(unique(na.omit(data[["RemovalBeforeNYear"]])))),
          multiple = T,
          options = list(
            `actions-box` = TRUE, size = 10,
            `selected-text-format` = "count > 2"
          )
        )
      })

      output$selectRemovalReason <- renderUI({
        pickerInput(ns("selectRemovalReason"),
          paste0("Filter Removal Reason(s)"),
          choices = as.character(
            sort(unique(data[["RemovalReason"]]))
          ),
          multiple = T,
          options = list(
            `actions-box` = TRUE, size = 10,
            `selected-text-format` = "count > 2"
          )
        )
      })

      output$selectFactor <- renderUI({
        pickerInput(ns("selectFactor"),
          "Select Factor",
          choices = list(
            None = c("None"),
            Factors = data %>% select(where(is.factor) & -ends_with("Id")) %>% names(),
            Logical = data %>% select(where(is.logical) & -ends_with("Id")) %>% names()
          ),
          selected = "None"
        )
      })

      output$selectLevels <- renderUI({
        req(input$selectFactor)
        req(input$selectFactor != "None")
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

      ## Plots --------------------------------------------------------------------
      output$removalsImplantPlot <- renderPlot({
        print("hi")
        overviewRemovalReasonPlot(
          data = data,
          removalReasons = input$selectRemovalReason,
          years = input$selectYears,
          factor = input$selectFactor,
          levels = input$selectLevels,
          showMean = "Mean" %in% input$implantPlotOptions
        )
      })
    }
  )
}
