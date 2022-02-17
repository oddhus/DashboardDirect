source("shiny-app/ClinicSuccessRatePlot.R")

clinicInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("selectClinic"), width = 12),
    actionBttn(ns("add"), "Add to report", style = "bordered", color = "warning")
  )
}

clinicSuccessRatePlotUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("successRatePlot"), height = 600) %>% withSpinner()
}

clinicServer <- function(id, data, plotInReport) {
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
                                  list(c("Factor" = isolate(input$selectFactor),
                                         "Levels" = isolate(paste(input$selectLevels, collapse = ";")),
                                         "AdditionalFactor" = isolate(input$selectAdditionalFactor),
                                         "AdditionalLevels" = isolate(paste(input$selectAdditionalLevels, collapse = ";")),
                                         "tab" = "Implant Survival")))
        }
      })
      
      
      ## Inputs -------------------------------------------------------------------
      output$selectClinic <- renderUI({
        pickerInput(ns("selectClinic"),
                    "Select Clinic",
                    choices = as.character(
                      sort(unique(data[["Clinic"]]))
                    ),
                    selected = "None"
        )
      })
      
      
      ## Plots --------------------------------------------------------------------
      output$successRatePlot <- renderPlot({
        clinicSuccesRatePlot(data = data)
      })
      
      
    }
  )
}
