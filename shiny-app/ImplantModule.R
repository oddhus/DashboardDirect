source("shiny-app/ImplantPlot.R")

implantInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("selectRemovalReason")),
    htmlOutput(ns("selectImplantName")),
    checkboxGroupButtons(
      inputId = ns("implantPlotOptions"),
      label = "Show",
      choices = c("LotNr"),
      status = "info",
      selected = NULL
    )
  )
}

implantPlotUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("removalsImplantPlot"), height = 600) %>% withSpinner()
}

implantServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      ## Inputs -------------------------------------------------------------------
      output$selectRemovalReason <- renderUI({
        pickerInput(ns("selectRemovalReason"),
          "Select Removal Reason(s)",
          choices = as.character(
            sort(unique(data[["RemovalReason"]]))
          ),
          multiple = T
        )
      })

      output$selectImplantName <- renderUI({
        pickerInput(ns("selectImplantName"),
          "Select Implant Name(s)",
          choices = as.character(
            sort(unique(data[["ImplantName"]]))
          ),
          multiple = T
        )
      })

      ## Plots --------------------------------------------------------------------
      output$removalsImplantPlot <- renderPlot({
        implantPlot(
          data,
          input$selectRemovalReason,
          input$selectImplantName,
          "LotNr" %in% input$implantPlotOptions
        )
      })
    }
  )
}
