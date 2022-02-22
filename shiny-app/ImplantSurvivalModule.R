source("shiny-app/ImplantSurvivalPlot.R")
source("shiny-app/ImplantLotNrSurvival.R")

implantSurvivalPlotInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("selectImplants")),
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
    ),
    actionBttn(ns("add"), "Add to report", style = "bordered", color = "warning")
  )
}

implantLotNrSurvivalPlotInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("selectLotNrImplants"))
    )
}

implantSurvivalPlotUI <- function(id){
  ns <- NS(id)
  plotOutput(ns("implantSurvivalPlot"), height = 650) %>% withSpinner()
}

implantLotNrSurvivalPlotUI <- function(id){
  ns <- NS(id)
  plotOutput(ns("implantLotNrSurvivalPlot"), height = 500) %>% withSpinner()
}

implantSurvivalServer <- function(id, data, plotInReport) {
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
                                  list(c("selectedImplants" = isolate(paste(input$selectedImplants, collapse = ";")),
                                         "firstYear" = isolate(input$selectFirst),
                                         "secondYear" = isolate(input$selectSecond),
                                         "selectedLotNrImplants" = isolate(paste(input$selectLotNrImplants, collapse = ";")),
                                         "tab" = "Implant Survival")))
        }
      })
      
      ## Inputs -------------------------------------------------------------------
      output$selectImplants <- renderUI({
        pickerInput(ns("selectImplants"),
                    "Filter Implant(s)",
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
      
      output$selectLotNrImplants <- renderUI({
        pickerInput(ns("selectLotNrImplants"),
                    "Select Implant(s)",
                    choices = if (isTruthy(input$selectImplants)) {
                      input$selectImplants
                    } else{
                      as.character(sort(unique(data[["ImplantName"]])))
                    } ,
                    multiple = T,
                    options = list(
                      `actions-box` = TRUE, size = 10,
                      `selected-text-format` = "count > 2"
                    )
        )
      })
      
      ## Plots --------------------------------------------------------------------
      
      
      output$implantSurvivalPlot <- renderPlot({
        req(isTruthy(input$selectFirst))
        req(isTruthy(input$selectSecond))
        
        implantSurvivalPlot(data = data,
                            selectedImplants = input$selectImplants,
                            firstYear = input$selectFirst,
                            secondYear = input$selectSecond)
      })
      
      output$implantLotNrSurvivalPlot <- renderPlot({
        req(isTruthy(input$selectFirst))
        req(isTruthy(input$selectSecond))
        
        implantLotNrSurvivalPlot(data = data,
                                 selectedImplants = input$selectLotNrImplants,
                                 firstYear = input$selectFirst,
                                 secondYear = input$selectSecond)
      })
      
    }
  )
}

