source("shiny-app/TimeSeriesPlot.R")

timeSeriesInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("selectRemovalReason")),
    htmlOutput(ns("selectImplantName")),
    htmlOutput(ns("selectClinic")),
    radioGroupButtons(
      inputId = ns("timeScale"),
      label = "Select timescale", 
      choices = c("month", "year"),
      status = "primary",
      selected = "year"
    ),
    actionBttn(ns("add"), "Add to report", style = "bordered", color = "warning")
  )
}

timeSeriesPlotUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("timeSeriesPlot"), height = 600) %>% withSpinner()
}

timeSeriesServer <- function(id, data, plotInReport) {
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
                                  list(c("Clinic" = isolate(paste(input$selectClinic, collapse = ";")),
                                         "RemovalReason" = isolate(paste(input$selectRemovalReason, collapse = ";")),
                                         "ImplantName" = isolate(paste(input$selectImplantName, collapse = ";")),
                                         "TimeScale" =  isolate(input$timeScale),
                                         "tab" = "timeSeries")))
        }
      })
      
      
      ## Inputs -------------------------------------------------------------------
      output$selectClinic <- renderUI({
        pickerInput(ns("selectClinic"),
                    "Select Clinic",
                    choices = as.character(
                      sort(unique(data[["Clinic"]]))
                    ),
                    multiple = T,
                    options = list(
                      `actions-box` = TRUE, size = 10,
                      `selected-text-format` = "count > 2"
                    )
        )
      })
      
      output$selectRemovalReason <- renderUI({
        pickerInput(ns("selectRemovalReason"),
                    "Select Removal Reason(s)",
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
      
      output$selectImplantName <- renderUI({
        pickerInput(ns("selectImplantName"),
                    "Select Implant Name(s)",
                    choices = as.character(
                      sort(unique(data[["ImplantName"]]))
                    ),
                    multiple = T,
                    options = list(
                      `actions-box` = TRUE, size = 10,
                      `selected-text-format` = "count > 2"
                    )
        )
      })
      
      ## Plots --------------------------------------------------------------------
      output$timeSeriesPlot <- renderPlot({
        print(input$timeScale,)
        req(isTruthy(input$timeScale))
        
        timeSeriesPlot(
          data,
          input$timeScale,
          input$selectClinic,
          input$selectRemovalReason,
          input$selectImplantName
        )
      })
    }
  )
}
