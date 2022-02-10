source("shiny-app/ImplantPlot.R")
source("shiny-app/ImplantPlotCompare.R")

implantInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    radioGroupButtons(
      inputId = ns("selectXAxis"),
      label = "X-axis",
      choices = c("Removals", "Implants"),
      status = "primary"
    ),
    htmlOutput(ns("selectRemovalReason")),
    htmlOutput(ns("selectImplantName")),
    htmlOutput(ns("selectClinic")),
    checkboxGroupButtons(
      inputId = ns("implantPlotOptions"),
      label = "Show",
      choices = c("LotNr", "Mean"),
      status = "info",
      selected = NULL
    ),
    actionBttn(ns("add"), "Add to report", style = "bordered", color = "warning")
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
                                  list(c("Clinic" = isolate(paste(input$selectClinic, collapse = ";")),
                                         "RemovalReason" = isolate(paste(input$selectRemovalReason, collapse = ";")),
                                         "ImplantName" = isolate(paste(input$selectImplantName, collapse = ";")),
                                         "ShowLotNr" = isolate("LotNr" %in% input$implantPlotOptions),
                                         "ShowMean" = isolate("Mean" %in% input$implantPlotOptions),
                                         "SelectXAxis" = isolate(input$selectXAxis),
                                         "tab" = "Implant")))
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
          paste0("Select Removal Reason", if("Removals" %in% input$selectXAxis) "(s)" else ""),
          choices = as.character(
            sort(unique(data[["RemovalReason"]]))
          ),
          multiple = "Removals" %in% input$selectXAxis,
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
      output$removalsImplantPlot <- renderPlot({
        req(input$selectXAxis)
        if("Removals" %in% input$selectXAxis){
          implantPlot(
            data,
            input$selectClinic,
            input$selectRemovalReason,
            input$selectImplantName,
            "LotNr" %in% input$implantPlotOptions,
            "Mean" %in% input$implantPlotOptions
          )
        } else {
          compareImplantsPlot(
            data,
            input$selectClinic,
            input$selectRemovalReason,
            input$selectImplantName,
            "LotNr" %in% input$implantPlotOptions,
            "Mean" %in% input$implantPlotOptions
          )
        }
        
      })
      
      
    }
  )
}
