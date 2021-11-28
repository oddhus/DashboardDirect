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
    ),
    actionBttn(ns("add"), "Add to report", style = "stretch", color = "warning")
  )
}

implantPlotUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("removalsImplantPlot"), height = 600) %>% withSpinner()
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
        if(input$confirm){
          plotInReport$dList <- c(isolate(plotInReport$dList),
                                  list(c("RemovalReason" = isolate(paste(input$selectRemovalReason, collapse = ";")),
                                         "ImplantName" = isolate(paste(input$selectImplantName, collapse = ";")),
                                         "showLotNr" = isolate("LotNr" %in% input$implantPlotOptions),
                                         "tab" = "Implant")))
        }
      })
      
      
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
